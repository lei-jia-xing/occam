-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: POSTGRES

-- Statements: 150

-- AST Expression Depth: 16

-- Injected Defect Case: pg_05

-- Seed: 15011



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2),
    col_4 JSONB
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 INT,
    col_3 VARCHAR(64),
    col_4 JSONB,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 BOOLEAN
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 BOOLEAN,
    col_3 JSONB,
    col_4 JSONB,
    ref_tbl_03_id INT,
    FOREIGN KEY (ref_tbl_03_id) REFERENCES tbl_03(id)
);

ALTER TABLE tbl_01 ADD COLUMN dyn_col_6 VARCHAR(32);

CREATE TYPE task_priority AS ENUM ('low', 'medium', 'high', 'critical');

ALTER TABLE tbl_02 DROP COLUMN ref_tbl_01_id;

SELECT
    -1.5 AS c_2,
    ((EXISTS (SELECT 1 FROM tbl_04 AS sub_3 WHERE TRUE) > (t_ref_1.col_1 AND (((LENGTH(((SELECT sub_13.col_3 FROM tbl_04 AS sub_13 LIMIT 1) < NULLIF((SELECT LENGTH(LENGTH((NOT ((-1 / NULLIF(sub_14.id, 0)))))) FROM dev_tasks AS sub_14 LIMIT 1), t_ref_1.col_1))) < t_ref_1.col_2) OR (((((((2147483647 <= t_ref_1.col_2) - (SELECT sub_15.col_1 FROM tbl_01 AS sub_15 LIMIT 1)) AND t_ref_1.col_2) OR '__fuzz_marker__') > ((TRUE::TEXT) + ((CASE WHEN (t_ref_1.col_1 * (((FALSE / NULLIF(FALSE, 0))::NUMERIC(10,2)) - t_ref_1.col_1)) THEN (CASE WHEN (t_ref_1.col_2::INT::TEXT) THEN ((t_ref_1.col_2 <> (-2147483648 >= t_ref_1.col_2)) >= UPPER((FALSE <= t_ref_1.col_1))) ELSE LOWER((t_ref_1.id::TEXT)) END) ELSE t_ref_1.col_1 END) >= NULLIF(t_ref_1.col_1, ((t_ref_1.col_1 IN (SELECT sub_16.id FROM tbl_04 AS sub_16)) + (t_ref_1.col_1 IN (SELECT sub_17.col_1 FROM tbl_04 AS sub_17))))))) + (t_ref_1.id <> t_ref_1.col_2)) AND ((CASE WHEN (LENGTH(t_ref_1.col_2) / NULLIF(((NULLIF(LOWER((SELECT sub_18.col_2 FROM tbl_03 AS sub_18 LIMIT 1)), (CASE WHEN (SELECT sub_19.col_1 FROM tbl_04 AS sub_19 LIMIT 1) THEN t_ref_1.id ELSE ((CASE WHEN t_ref_1.id THEN t_ref_1.col_2 ELSE t_ref_1.id END) OR t_ref_1.col_2) END)) * (t_ref_1.id::BOOLEAN)) + (t_ref_1.col_2 BETWEEN -2147483648 AND -2147483648)), 0)) THEN COALESCE(((CASE WHEN ((ABS(t_ref_1.col_1) - (SELECT (SELECT FALSE FROM tbl_02 AS inn_21 LIMIT 1) FROM tbl_04 AS sub_20 LIMIT 1)) AND (LOWER((t_ref_1.col_2 + t_ref_1.id)) + (SELECT sub_22.col_2 FROM tbl_01 AS sub_22 LIMIT 1))) THEN ((t_ref_1.id < ((t_ref_1.col_1::INT) IS NOT NULL)) <= (((t_ref_1.col_2 IN (SELECT sub_23.col_1 FROM tbl_01 AS sub_23)) * (t_ref_1.col_2 / NULLIF(t_ref_1.id, 0))) * ((t_ref_1.id OR NULL) + t_ref_1.col_2))) ELSE EXISTS (SELECT 1 FROM tbl_01 AS sub_24 WHERE FALSE) END)::INT), ABS(NULL)) ELSE ((999999999.99 <> '') < (t_ref_1.col_2 IN (SELECT sub_25.ref_tbl_03_id FROM tbl_04 AS sub_25))) END) AND ((t_ref_1.col_2 IN (SELECT sub_27.col_1 FROM tbl_02 AS sub_27)) * FALSE)))) * (((((((t_ref_1.id >= COALESCE((CASE WHEN EXISTS (SELECT 1 FROM dev_tasks AS sub_31 WHERE sub_31.payload) THEN t_ref_1.col_1 ELSE ((NULL - t_ref_1.col_2) >= t_ref_1.col_1) END), (EXISTS (SELECT 1 FROM tbl_01 AS sub_32 WHERE -1e-5)::INT::TEXT))) BETWEEN 42 AND 9223372036854775807)::NUMERIC(10,2)) - (((SELECT sub_33.payload FROM dev_tasks AS sub_33 LIMIT 1) + LOWER((ABS((ABS(t_ref_1.id) < UPPER(t_ref_1.id)))::NUMERIC(10,2)))) IS NULL))::BOOLEAN)::TEXT) / NULLIF(UPPER((EXISTS (SELECT 1 FROM tbl_04 AS sub_34 WHERE sub_34.col_4) + (FALSE::TEXT::VARCHAR(32)))), 0))))) IS NULL) AS c_39,
    TRUE AS c_40
FROM tbl_03 AS t_ref_1
WHERE 'ref_val'
LIMIT 40;

CREATE INDEX idx_tbl_04_col_2_1 ON tbl_04 (col_2);

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (244930, -1e-5, 42, 'escaped''quote', '[]');

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_01 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO dev_tasks (id, payload) VALUES (706519, '');
INSERT INTO tbl_03 (id, col_1, col_2) VALUES (809153, TRUE, TRUE);
COMMIT;

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2) VALUES (310850, FALSE, TRUE);
COMMIT;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (360259, TRUE, 'escaped''quote', 1.0, '[]', 'alpha_beta_gamma');

DELETE FROM dev_tasks WHERE id = 9857;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (416479, 0.0000000001, 1, 'chaos', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

UPDATE tbl_04 SET col_2 = TRUE WHERE id > 7;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (123632, FALSE, 'alpha_beta_gamma', 999999999.99, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '');

SELECT
    COALESCE(t_ref_41.col_1, (CASE WHEN ABS(COALESCE(NULLIF(UPPER(t_ref_41.id), ((t_ref_41.id = (EXISTS (SELECT 1 FROM tbl_04 AS sub_42 WHERE sub_42.ref_tbl_03_id)::TEXT)) < UPPER((CASE WHEN UPPER(t_ref_41.col_2) THEN t_ref_41.id ELSE (t_ref_41.col_1 * (SELECT (SELECT inn_44.col_3 FROM tbl_01 AS inn_44 LIMIT 1) FROM tbl_02 AS sub_43 LIMIT 1)) END)))), t_ref_41.col_1)) THEN (SELECT ((NULLIF((((NOT (((CASE WHEN (SELECT (CASE WHEN sub_46.id THEN sub_46.col_2 ELSE sub_46.col_4 END) FROM tbl_04 AS sub_46 LIMIT 1) THEN (((SELECT (SELECT inn_48.id FROM dev_tasks AS inn_48 LIMIT 1) FROM tbl_01 AS sub_47 LIMIT 1)::INT::TEXT) OR ((NOT ('alpha_beta_gamma')) + (sub_45.payload <= NULL))) ELSE NULLIF(((sub_45.payload + TRUE) / NULLIF((sub_45.payload IN (SELECT sub_49.col_2 FROM tbl_01 AS sub_49)), 0)), ((sub_45.id <= TRUE) / NULLIF(-1.5, 0))) END) IS NULL))) - ABS(sub_45.id))::NUMERIC(10,2)), (CASE WHEN (NULL + (EXISTS (SELECT 1 FROM tbl_02 AS sub_50 WHERE sub_50.col_2) * 'ref_val')) THEN ((sub_45.payload IN (SELECT sub_51.id FROM dev_tasks AS sub_51)) < LENGTH((NULL <> (SELECT ((1::TEXT) OR NULLIF(sub_53.col_3, sub_53.col_2)) FROM tbl_02 AS sub_53 LIMIT 1)))) ELSE 'escaped''quote' END)) BETWEEN 42 AND -1)::BOOLEAN) FROM dev_tasks AS sub_45 LIMIT 1) ELSE 999999999.99 END)) AS c_54
FROM tbl_03 AS t_ref_41
WHERE t_ref_41.col_2
LIMIT 15;

INSERT INTO dev_tasks (id, payload) VALUES (641760, 'alpha_beta_gamma');

BEGIN;
INSERT INTO dev_tasks (id, payload) VALUES (989509, '__fuzz_marker__');
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (946234, 1e10, -9223372036854775808, '', '{}');
COMMIT;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (850983, FALSE, 'escaped''quote', 999999999.99, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'ref_val');

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_01 WHERE id IS NOT NULL;

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    col_3 VARCHAR(64)
);

SELECT
    t_ref_55.col_1 AS c_56,
    FALSE AS c_57
FROM tbl_02 AS t_ref_55 LEFT JOIN tbl_03 AS j_ref_58 ON t_ref_55.id = j_ref_58.id
WHERE ((((UPPER(((t_ref_55.col_1 >= FALSE) * t_ref_55.col_4)) / NULLIF((t_ref_55.col_3 - ABS(NULLIF((((((-1 IN (SELECT sub_59.id FROM tbl_04 AS sub_59))::BOOLEAN)::INT::TEXT) < LENGTH(-9223372036854775808)) / NULLIF(t_ref_55.col_2, 0)), 0.0))), 0))::TEXT) IS NOT NULL) = (ABS((((NOT (COALESCE((SELECT (SELECT (((('__fuzz_marker__' * '') BETWEEN 0 AND -1) = (SELECT sub_62.id FROM tbl_02 AS sub_62 LIMIT 1)) / NULLIF((SELECT (0 + FALSE) FROM tbl_03 AS sub_63 LIMIT 1), 0)) FROM tbl_02 AS sub_61 LIMIT 1) FROM tbl_05 AS sub_60 LIMIT 1), ((SELECT (((((sub_64.col_2 / NULLIF(sub_64.col_1, 0)) IS NOT NULL) IS NULL)::NUMERIC(10,2))::TEXT) FROM tbl_03 AS sub_64 LIMIT 1)::INT)))) IS NULL) < ((-1.5 - FALSE) - (((('escaped''quote' <> ((SELECT (SELECT inn_69.col_1 FROM tbl_01 AS inn_69 LIMIT 1) FROM tbl_05 AS sub_65 LIMIT 1) < t_ref_55.id)) / NULLIF(UPPER(((SELECT ((EXISTS (SELECT 1 FROM tbl_04 AS sub_71 WHERE sub_71.col_1) / NULLIF(UPPER(TRUE), 0)) + (SELECT (SELECT FALSE FROM tbl_03 AS inn_73 LIMIT 1) FROM dev_tasks AS sub_72 LIMIT 1)) FROM tbl_04 AS sub_70 LIMIT 1) / NULLIF(t_ref_55.col_3, 0))), 0))::INT::TEXT) OR (((t_ref_55.col_1 * ((t_ref_55.col_1 >= (SELECT (EXISTS (SELECT 1 FROM dev_tasks AS sub_75 WHERE sub_75.payload) > (sub_74.col_1::NUMERIC(10,2))) FROM tbl_05 AS sub_74 LIMIT 1)) OR ((NOT (ABS((t_ref_55.col_4::TEXT::VARCHAR(32))))) >= (t_ref_55.col_1 - EXISTS (SELECT 1 FROM tbl_03 AS sub_76 WHERE 'escaped''quote'))))) * UPPER(EXISTS (SELECT 1 FROM tbl_02 AS sub_77 WHERE sub_77.col_4))) - (t_ref_55.col_2 IN (SELECT sub_80.col_3 FROM tbl_01 AS sub_80))))))) OR NULLIF((t_ref_55.id / NULLIF('ref_val', 0)), ((t_ref_55.col_2 <> t_ref_55.col_3)::INT::TEXT))))
LIMIT 17;

ALTER TYPE task_priority ADD VALUE 'urgent' BEFORE 'critical';

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (755654, TRUE, '', '__fuzz_marker__');

UPDATE tbl_04 SET col_4 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 10;

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 VARCHAR(64),
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

UPDATE dev_tasks SET payload = 'alpha_beta_gamma' WHERE id > 25;

CREATE INDEX idx_tbl_04_col_2_2 ON tbl_04 (col_2);

UPDATE tbl_03 SET col_1 = FALSE WHERE id > 68;

UPDATE tbl_03 SET col_1 = TRUE WHERE id > 9;

INSERT INTO tbl_03 (id, col_1, col_2) VALUES (647332, FALSE, FALSE);

INSERT INTO dev_tasks (id, payload) VALUES (491035, '');

INSERT INTO tbl_03 (id, col_1, col_2) VALUES (653716, FALSE, TRUE);

UPDATE tbl_02 SET col_3 = 'ref_val' WHERE id > 62;

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2) VALUES (691241, TRUE, TRUE);
INSERT INTO tbl_06 (id, col_1, col_2, ref_tbl_02_id) VALUES (404602, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '__fuzz_marker__', 100);
COMMIT;

ALTER TABLE tbl_04 ADD COLUMN dyn_col_7 VARCHAR(32);

UPDATE tbl_05 SET col_2 = 'chaos' WHERE id > 0;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 INT,
    ref_tbl_05_id INT,
    FOREIGN KEY (ref_tbl_05_id) REFERENCES tbl_05(id)
);

SELECT
    (-1.5 = ((1.0 BETWEEN 9223372036854775807 AND 0) < COALESCE((SELECT (SELECT 3.14159265 FROM dev_tasks AS inn_96 LIMIT 1) FROM tbl_04 AS sub_85 LIMIT 1), 100))) AS c_97,
    ((NOT (COALESCE(TRUE, (SELECT (SELECT inn_99.id FROM dev_tasks AS inn_99 LIMIT 1) FROM tbl_04 AS sub_98 LIMIT 1)))) >= t_ref_84.col_1) AS c_100
FROM tbl_03 AS t_ref_84
WHERE (SELECT (FALSE IN (SELECT sub_102.payload FROM dev_tasks AS sub_102)) FROM tbl_05 AS sub_101 LIMIT 1)
LIMIT 15;

INSERT INTO tbl_03 (id, col_1, col_2) VALUES (420596, FALSE, TRUE);

INSERT INTO tbl_06 (id, col_1, col_2, ref_tbl_02_id) VALUES (575282, '[]', 'alpha_beta_gamma', 0);

SELECT
    (t_ref_103.id * t_ref_103.col_1) AS c_104,
    ((((t_ref_103.ref_tbl_05_id IN (SELECT sub_105.col_2 FROM tbl_06 AS sub_105)) / NULLIF(('__fuzz_marker__' / NULLIF((t_ref_103.id + (SELECT (SELECT inn_107.col_3 FROM tbl_05 AS inn_107 LIMIT 1) FROM tbl_03 AS sub_106 LIMIT 1)), 0)), 0)) <> (SELECT (((SELECT EXISTS (SELECT 1 FROM tbl_04 AS sub_110 WHERE sub_110.ref_tbl_03_id) FROM tbl_04 AS sub_109 LIMIT 1)::NUMERIC(10,2)) / NULLIF(((sub_108.ref_tbl_02_id = EXISTS (SELECT 1 FROM tbl_04 AS sub_111 WHERE -9223372036854775808)) <> sub_108.id), 0)) FROM tbl_06 AS sub_108 LIMIT 1))::INT::TEXT) AS c_113
FROM tbl_07 AS t_ref_103
WHERE 0.0
LIMIT 8;

SELECT
    EXISTS (SELECT 1 FROM dev_tasks AS sub_115 WHERE TRUE) AS c_116
FROM tbl_06 AS t_ref_114
WHERE (SELECT (SELECT 1e10 FROM tbl_01 AS inn_134 LIMIT 1) FROM tbl_03 AS sub_117 LIMIT 1)
LIMIT 21;

CREATE TABLE dev_tasks (
    task_id INT PRIMARY KEY,
    title VARCHAR(128) NOT NULL,
    priority task_priority NOT NULL,
    owner_id INT
);

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (106549, FALSE, 'alpha_beta_gamma', -1e-5, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'escaped''quote');

SELECT
    (FALSE IN (SELECT sub_136.payload FROM dev_tasks AS sub_136)) AS c_143,
    (EXISTS (SELECT 1 FROM tbl_07 AS sub_144 WHERE NULL) <= (CASE WHEN (SELECT (SELECT inn_146.col_3 FROM tbl_04 AS inn_146 LIMIT 1) FROM tbl_05 AS sub_145 LIMIT 1) THEN (SELECT EXISTS (SELECT 1 FROM tbl_06 AS sub_148 WHERE sub_148.col_2) FROM tbl_07 AS sub_147 LIMIT 1) ELSE (t_ref_135.id::BOOLEAN) END)) AS c_158
FROM tbl_05 AS t_ref_135 LEFT JOIN tbl_03 AS j_ref_159 ON t_ref_135.id = j_ref_159.id
WHERE (SELECT (sub_160.col_2 IN (SELECT sub_161.col_1 FROM tbl_06 AS sub_161)) FROM tbl_07 AS sub_160 LIMIT 1)
LIMIT 4;

CREATE INDEX idx_tbl_04_ref_tbl_03_id_3 ON tbl_04 (ref_tbl_03_id);

CREATE INDEX idx_tbl_01_col_3_4 ON tbl_01 (col_3);

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (7574, FALSE, 'escaped''quote', 'escaped''quote');

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (671322, -1e-5, -2147483648, '__fuzz_marker__', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
COMMIT;

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (621445, TRUE, 'alpha_beta_gamma', '__fuzz_marker__');

SELECT
    ((((NOT (t_ref_174.id)) * ((SELECT (SELECT 'chaos' FROM tbl_04 AS inn_176 LIMIT 1) FROM tbl_01 AS sub_175 LIMIT 1) + (NULLIF(EXISTS (SELECT 1 FROM dev_tasks AS sub_177 WHERE sub_177.id), t_ref_174.id) - t_ref_174.payload))) BETWEEN -1 AND -1) + (CASE WHEN ((NULLIF((-9223372036854775808::NUMERIC(10,2)), FALSE) / NULLIF(((SELECT (SELECT inn_186.col_3 FROM tbl_01 AS inn_186 LIMIT 1) FROM tbl_06 AS sub_179 LIMIT 1) - (NOT ((EXISTS (SELECT 1 FROM tbl_04 AS sub_187 WHERE sub_187.col_1) IS NOT NULL)))), 0)) - (NULLIF((t_ref_174.payload / NULLIF((t_ref_174.id IN (SELECT sub_188.col_1 FROM tbl_06 AS sub_188)), 0)), t_ref_174.id) + NULLIF(COALESCE(((SELECT (SELECT inn_191.id FROM tbl_06 AS inn_191 LIMIT 1) FROM tbl_02 AS sub_189 LIMIT 1) IS NULL), t_ref_174.id), (-1.5 / NULLIF(NULLIF((1 IS NOT NULL), (1 * (CASE WHEN NULL THEN FALSE ELSE ((t_ref_174.id IN (SELECT sub_192.col_1 FROM tbl_07 AS sub_192)) / NULLIF((CASE WHEN (((CASE WHEN (t_ref_174.payload / NULLIF(0, 0)) THEN (0.0 + t_ref_174.payload) ELSE (t_ref_174.id::INT) END) + ((-1e-5 < 'escaped''quote') * EXISTS (SELECT 1 FROM dev_tasks AS sub_193 WHERE 'ref_val'))) + ((SELECT (SELECT NULL FROM tbl_03 AS inn_195 LIMIT 1) FROM tbl_05 AS sub_194 LIMIT 1) OR TRUE)) THEN ((t_ref_174.id + ((TRUE IS NOT NULL)::INT::TEXT)) = (LOWER((999999999.99::INT::TEXT)) = (SELECT sub_196.col_1 FROM tbl_02 AS sub_196 LIMIT 1))) ELSE t_ref_174.id END), 0)) END))), 0))))) THEN t_ref_174.payload ELSE -1 END)) AS c_197,
    (CASE WHEN (TRUE::BOOLEAN) THEN 'ref_val' ELSE (NULL::INT) END) AS c_198
FROM dev_tasks AS t_ref_174 LEFT JOIN tbl_03 AS j_ref_199 ON t_ref_174.id = j_ref_199.id
WHERE (TRUE AND (((TRUE::NUMERIC(10,2)) - (t_ref_174.payload::NUMERIC(10,2))) + LOWER((((t_ref_174.payload IN (SELECT sub_200.col_2 FROM tbl_06 AS sub_200)) / NULLIF(ABS(((NOT ((UPPER((t_ref_174.payload IN (SELECT sub_201.col_2 FROM tbl_07 AS sub_201))) OR (SELECT sub_205.col_2 FROM tbl_06 AS sub_205 LIMIT 1)))) <= UPPER((ABS((UPPER('chaos') + (SELECT (CASE WHEN (sub_206.col_1 = TRUE) THEN (sub_206.col_4::BOOLEAN) ELSE COALESCE(sub_206.id, sub_206.col_1) END) FROM tbl_02 AS sub_206 LIMIT 1))) <> ((CASE WHEN (-1 IN (SELECT sub_207.id FROM tbl_02 AS sub_207)) THEN 3.14159265 ELSE LOWER(((SELECT 'ref_val' FROM dev_tasks AS sub_209 LIMIT 1) / NULLIF((t_ref_174.id - t_ref_174.payload), 0))) END) > (t_ref_174.id * (((t_ref_174.id::NUMERIC(10,2))::BOOLEAN) AND t_ref_174.payload))))))), 0))::BOOLEAN))))
LIMIT 11;

INSERT INTO tbl_06 (id, col_1, col_2, ref_tbl_02_id) VALUES (910205, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'ref_val', 0);

SELECT
    (LENGTH(LENGTH(t_ref_210.payload))::INT::TEXT) AS c_211,
    (NOT ((((EXISTS (SELECT 1 FROM tbl_01 AS sub_212 WHERE sub_212.col_4)::TEXT) >= ABS(t_ref_210.payload))::TEXT))) AS c_213
FROM dev_tasks AS t_ref_210
WHERE (CASE WHEN (42 = ((LOWER((((((t_ref_210.payload::INT) < (SELECT (SELECT inn_218.col_2 FROM tbl_05 AS inn_218 LIMIT 1) FROM tbl_07 AS sub_214 LIMIT 1)) BETWEEN 42 AND -1) IS NULL) * (CASE WHEN COALESCE((SELECT (sub_219.id * (SELECT (SELECT inn_221.col_3 FROM tbl_05 AS inn_221 LIMIT 1) FROM tbl_06 AS sub_220 LIMIT 1)) FROM tbl_07 AS sub_219 LIMIT 1), ((((SELECT ((sub_222.ref_tbl_02_id - 999999999.99)::BOOLEAN) FROM tbl_06 AS sub_222 LIMIT 1)::TEXT) <> (NULLIF(t_ref_210.payload, (SELECT (SELECT NULL FROM tbl_06 AS inn_224 LIMIT 1) FROM tbl_07 AS sub_223 LIMIT 1)) / NULLIF(((((t_ref_210.id <= -1) / NULLIF((t_ref_210.id::TEXT::VARCHAR(32)), 0)) * ((t_ref_210.payload - TRUE) - t_ref_210.id)) + (NOT ((FALSE::TEXT)))), 0))) / NULLIF((SELECT sub_225.col_1 FROM tbl_03 AS sub_225 LIMIT 1), 0))) THEN (t_ref_210.payload IN (SELECT sub_226.col_1 FROM tbl_05 AS sub_226)) ELSE (EXISTS (SELECT 1 FROM dev_tasks AS sub_227 WHERE 9223372036854775807) / NULLIF(COALESCE((COALESCE((SELECT (SELECT inn_229.col_1 FROM tbl_03 AS inn_229 LIMIT 1) FROM tbl_06 AS sub_228 LIMIT 1), 1) = EXISTS (SELECT 1 FROM tbl_05 AS sub_230 WHERE -1e-5)), ((UPPER(NULLIF((t_ref_210.id * t_ref_210.id), t_ref_210.payload)) - (FALSE OR ((42 < (42 + 9223372036854775807)) * ((1 * t_ref_210.id)::TEXT)))) / NULLIF(NULL, 0))), 0)) END))) / NULLIF((ABS(('chaos' + (NOT ((SELECT ((sub_231.col_2::BOOLEAN)::NUMERIC(10,2)) FROM tbl_02 AS sub_231 LIMIT 1)))))::NUMERIC(10,2)), 0)) - ((((SELECT (((((sub_232.col_3 IN (SELECT sub_233.col_2 FROM tbl_03 AS sub_233)) * ((CASE WHEN (sub_232.col_3 >= 1e10) THEN sub_232.col_1 ELSE 'escaped''quote' END) = (SELECT (SELECT inn_235.dyn_col_7 FROM tbl_04 AS inn_235 LIMIT 1) FROM dev_tasks AS sub_234 LIMIT 1))) + ((SELECT (SELECT sub_237.col_2 FROM tbl_06 AS sub_237 LIMIT 1) FROM dev_tasks AS sub_236 LIMIT 1) + (LENGTH(FALSE) * ((sub_232.col_3 - sub_232.col_4)::INT))))::BOOLEAN) * COALESCE((EXISTS (SELECT 1 FROM tbl_05 AS sub_238 WHERE NULL) / NULLIF(ABS((FALSE + (1e10 IN (SELECT sub_240.id FROM tbl_03 AS sub_240)))), 0)), (NULL::INT))) FROM tbl_02 AS sub_232 LIMIT 1)::INT) < t_ref_210.id) - NULLIF(t_ref_210.payload, (t_ref_210.id - ((CASE WHEN LENGTH((SELECT (LENGTH(UPPER(sub_241.col_1)) OR sub_241.col_4) FROM tbl_02 AS sub_241 LIMIT 1)) THEN NULLIF(NULLIF((((TRUE - ABS(('escaped''quote'::INT::TEXT))) OR ABS((SELECT 0.0 FROM dev_tasks AS sub_242 LIMIT 1))) * (CASE WHEN (SELECT (SELECT TRUE FROM tbl_05 AS inn_244 LIMIT 1) FROM tbl_03 AS sub_243 LIMIT 1) THEN LOWER(t_ref_210.payload) ELSE (SELECT (SELECT inn_248.ref_tbl_02_id FROM tbl_06 AS inn_248 LIMIT 1) FROM dev_tasks AS sub_245 LIMIT 1) END)), COALESCE((LOWER(((CASE WHEN 1e10 THEN t_ref_210.id ELSE t_ref_210.payload END) + (t_ref_210.id - 'chaos'))) / NULLIF(t_ref_210.id, 0)), ((NULLIF(LENGTH(t_ref_210.payload), (t_ref_210.id <> t_ref_210.payload)) / NULLIF((t_ref_210.payload IN (SELECT sub_249.col_1 FROM tbl_05 AS sub_249)), 0)) OR EXISTS (SELECT 1 FROM tbl_04 AS sub_250 WHERE 1e10)))), EXISTS (SELECT 1 FROM dev_tasks AS sub_251 WHERE sub_251.payload)) ELSE (SELECT (SELECT 9223372036854775807 FROM tbl_01 AS inn_256 LIMIT 1) FROM tbl_05 AS sub_252 LIMIT 1) END) BETWEEN -2147483648 AND 1)))))) THEN t_ref_210.payload ELSE EXISTS (SELECT 1 FROM tbl_03 AS sub_257 WHERE 'ref_val') END)
LIMIT 3;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    col_3 JSONB,
    col_4 JSONB
);

INSERT INTO dev_tasks (id, payload) VALUES (911737, '__fuzz_marker__');

CREATE INDEX idx_tbl_04_col_4_5 ON tbl_04 (col_4);

SELECT
    EXISTS (SELECT 1 FROM tbl_02 AS sub_267 WHERE sub_267.col_1) AS c_268
FROM tbl_05 AS t_ref_266 LEFT JOIN tbl_02 AS j_ref_269 ON t_ref_266.id = j_ref_269.id
WHERE ((((EXISTS (SELECT 1 FROM tbl_06 AS sub_270 WHERE sub_270.col_1) >= (t_ref_266.col_1 IN (SELECT sub_275.col_2 FROM tbl_06 AS sub_275))) - (NULLIF((FALSE IN (SELECT sub_276.col_1 FROM tbl_05 AS sub_276)), ((((((((t_ref_266.col_2 IS NOT NULL) - (((t_ref_266.col_1 / NULLIF(t_ref_266.col_2, 0)) + (NULL < t_ref_266.col_2)) OR EXISTS (SELECT 1 FROM tbl_07 AS sub_277 WHERE sub_277.col_1))) = t_ref_266.id) / NULLIF(t_ref_266.col_3, 0))::TEXT) OR (SELECT (SELECT NULL FROM dev_tasks AS inn_279 LIMIT 1) FROM tbl_04 AS sub_278 LIMIT 1)) BETWEEN 0 AND 42) AND (t_ref_266.id IN (SELECT sub_280.col_3 FROM tbl_08 AS sub_280))))::NUMERIC(10,2))) > (SELECT (SELECT 'ref_val' FROM tbl_01 AS sub_285 LIMIT 1) FROM tbl_06 AS sub_284 LIMIT 1))::INT::TEXT)
LIMIT 13;

UPDATE tbl_01 SET col_1 = FALSE WHERE id > 9;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 BOOLEAN
);

SELECT
    ((FALSE / NULLIF((t_ref_286.col_2 IN (SELECT sub_287.id FROM tbl_04 AS sub_287)), 0)) < (NULL = (SELECT (SELECT FALSE FROM dev_tasks AS inn_310 LIMIT 1) FROM tbl_01 AS sub_303 LIMIT 1))) AS c_311,
    LOWER(COALESCE((EXISTS (SELECT 1 FROM tbl_04 AS sub_312 WHERE 'escaped''quote') AND NULL), (t_ref_286.col_2 IN (SELECT sub_315.col_4 FROM tbl_02 AS sub_315)))) AS c_316,
    NULLIF(t_ref_286.col_1, UPPER((NOT ((t_ref_286.id BETWEEN 2147483647 AND 9223372036854775807))))) AS c_317
FROM tbl_03 AS t_ref_286 LEFT JOIN tbl_09 AS j_ref_318 ON t_ref_286.id = j_ref_318.id
WHERE ((((t_ref_286.col_2 + (SELECT (SELECT TRUE FROM tbl_06 AS inn_324 LIMIT 1) FROM tbl_06 AS sub_319 LIMIT 1)) >= (t_ref_286.col_1 = (((CASE WHEN (((NOT ((t_ref_286.col_1::INT))) OR ((((t_ref_286.col_1 IN (SELECT sub_325.id FROM tbl_06 AS sub_325)) AND 100) >= ((((FALSE::INT::TEXT) = 'ref_val') IS NULL) BETWEEN 9223372036854775807 AND -1)) OR (COALESCE(NULL, (CASE WHEN ((SELECT (SELECT inn_327.col_1 FROM tbl_07 AS inn_327 LIMIT 1) FROM tbl_04 AS sub_326 LIMIT 1)::INT::TEXT) THEN (EXISTS (SELECT 1 FROM tbl_02 AS sub_328 WHERE sub_328.id) * t_ref_286.col_2) ELSE ((1e10 + TRUE) BETWEEN 0 AND 2147483647) END)) AND EXISTS (SELECT 1 FROM tbl_07 AS sub_329 WHERE TRUE)))) < t_ref_286.col_1) THEN ((SELECT TRUE FROM tbl_05 AS sub_330 LIMIT 1)::TEXT::VARCHAR(32)) ELSE ((SELECT (SELECT sub_332.id FROM tbl_06 AS sub_332 LIMIT 1) FROM tbl_02 AS sub_331 LIMIT 1) <= EXISTS (SELECT 1 FROM tbl_02 AS sub_333 WHERE TRUE)) END) / NULLIF((t_ref_286.id::NUMERIC(10,2)), 0)) - (((LOWER(((LOWER((100 + (t_ref_286.col_1 / NULLIF((t_ref_286.col_1 IN (SELECT sub_334.col_2 FROM tbl_03 AS sub_334)), 0)))) <> ('' > t_ref_286.col_1))::INT)) * (t_ref_286.id IN (SELECT sub_335.col_1 FROM tbl_08 AS sub_335))) OR (CASE WHEN (CASE WHEN ('ref_val'::TEXT) THEN (t_ref_286.col_1 IN (SELECT sub_336.col_3 FROM tbl_07 AS sub_336)) ELSE ((SELECT -9223372036854775808 FROM tbl_03 AS sub_339 LIMIT 1) AND (t_ref_286.col_2 / NULLIF((9223372036854775807 / NULLIF((SELECT (SELECT inn_341.id FROM tbl_05 AS inn_341 LIMIT 1) FROM tbl_07 AS sub_340 LIMIT 1), 0)), 0))) END) THEN (((((((t_ref_286.col_2 < FALSE) AND NULLIF(TRUE, 100)) < (SELECT sub_342.col_1 FROM tbl_05 AS sub_342 LIMIT 1)) / NULLIF(((CASE WHEN (t_ref_286.col_2 = t_ref_286.id) THEN 'alpha_beta_gamma' ELSE (t_ref_286.id IN (SELECT sub_343.col_1 FROM tbl_06 AS sub_343)) END) >= EXISTS (SELECT 1 FROM tbl_02 AS sub_344 WHERE sub_344.col_4)), 0))::INT) + 'chaos') + t_ref_286.col_1) ELSE t_ref_286.col_2 END))::INT)))) * ((CASE WHEN LENGTH(UPPER(COALESCE(((((NOT (((SELECT LOWER(sub_345.id) FROM tbl_09 AS sub_345 LIMIT 1) - NULL))) + LOWER((SELECT (SELECT inn_347.col_4 FROM tbl_01 AS inn_347 LIMIT 1) FROM tbl_04 AS sub_346 LIMIT 1))) <= t_ref_286.col_1) < ((ABS((LOWER(((t_ref_286.id * 42) / NULLIF(UPPER(t_ref_286.col_1), 0))) <= (LENGTH(t_ref_286.col_1)::TEXT))) / NULLIF(9223372036854775807, 0))::BOOLEAN)), NULL))) THEN EXISTS (SELECT 1 FROM tbl_07 AS sub_348 WHERE sub_348.col_1) ELSE (TRUE IS NOT NULL) END) / NULLIF((((t_ref_286.col_2 IN (SELECT sub_349.col_2 FROM tbl_07 AS sub_349))::TEXT::VARCHAR(32))::INT::TEXT), 0))) * (((CASE WHEN (CASE WHEN t_ref_286.col_2 THEN (((NULL IN (SELECT sub_361.col_3 FROM tbl_07 AS sub_361)) - (EXISTS (SELECT 1 FROM tbl_01 AS sub_366 WHERE sub_366.col_4) OR (EXISTS (SELECT 1 FROM tbl_09 AS sub_367 WHERE -9223372036854775808) - t_ref_286.col_1)))::TEXT::VARCHAR(32)) ELSE ('__fuzz_marker__' IN (SELECT sub_370.col_2 FROM tbl_07 AS sub_370)) END) THEN (UPPER((CASE WHEN (SELECT ABS(('__fuzz_marker__' IN (SELECT sub_374.col_1 FROM tbl_05 AS sub_374))) FROM dev_tasks AS sub_373 LIMIT 1) THEN (NOT (NULLIF(t_ref_286.id, ((((SELECT (SELECT inn_379.col_1 FROM tbl_05 AS inn_379 LIMIT 1) FROM dev_tasks AS sub_377 LIMIT 1) / NULLIF((NOT (t_ref_286.id)), 0))::INT::TEXT) BETWEEN 2147483647 AND 100)))) ELSE t_ref_286.id END)) - ((SELECT UPPER(sub_380.col_4) FROM tbl_08 AS sub_380 LIMIT 1) OR (t_ref_286.col_2 IN (SELECT sub_381.id FROM tbl_01 AS sub_381)))) ELSE ((((FALSE / NULLIF((CASE WHEN (NULL::NUMERIC(10,2)) THEN ((((1.0 < (t_ref_286.id * (SELECT (SELECT inn_386.col_2 FROM tbl_04 AS inn_386 LIMIT 1) FROM tbl_05 AS sub_385 LIMIT 1)))::INT::TEXT)::INT::TEXT) > (EXISTS (SELECT 1 FROM tbl_02 AS sub_387 WHERE 'alpha_beta_gamma') AND LOWER(NULL))) ELSE ((FALSE OR NULL) * (((LENGTH((t_ref_286.col_2 - t_ref_286.id)) - (NULL AND (FALSE OR t_ref_286.id))) BETWEEN 1 AND -2147483648) - (SELECT (SELECT FALSE FROM tbl_06 AS inn_390 LIMIT 1) FROM dev_tasks AS sub_389 LIMIT 1))) END), 0)) IS NOT NULL) - ((SELECT (SELECT (sub_392.col_2 <= (CASE WHEN (SELECT sub_393.col_2 FROM tbl_09 AS sub_393 LIMIT 1) THEN ((NOT (1.0)) / NULLIF(sub_392.col_1, 0)) ELSE LOWER((sub_392.ref_tbl_02_id::INT)) END)) FROM tbl_06 AS sub_392 LIMIT 1) FROM tbl_07 AS sub_391 LIMIT 1) BETWEEN -1 AND -1)) BETWEEN 9223372036854775807 AND 9223372036854775807) END) - (SELECT (SELECT inn_395.col_3 FROM tbl_05 AS inn_395 LIMIT 1) FROM tbl_09 AS sub_394 LIMIT 1)) = (NULL - ((t_ref_286.col_2 IN (SELECT sub_396.col_2 FROM tbl_03 AS sub_396)) IS NULL))))
LIMIT 39;

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSONB
);

UPDATE tbl_07 SET col_2 = -1.5 WHERE id > 49;

ALTER TABLE tbl_05 DROP COLUMN col_3;

INSERT INTO dev_tasks (task_id, title, priority, owner_id) VALUES
    (1, 'Fix auth bypass', 'critical', 101),
    (2, 'Refactor CSS', 'low', 102),
    (3, 'Security patch kernel', 'urgent', 101),
    (4, 'Update readme', 'medium', 103);

UPDATE tbl_04 SET col_1 = '{}' WHERE id > 25;

INSERT INTO tbl_10 (id, col_1, col_2) VALUES (299332, 3.14159265, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

UPDATE dev_tasks SET payload = 'ref_val' WHERE id > 30;

INSERT INTO tbl_10 (id, col_1, col_2) VALUES (167015, 0.0, '[]');

SELECT
    (SELECT (SELECT inn_401.col_4 FROM tbl_01 AS inn_401 LIMIT 1) FROM tbl_06 AS sub_400 LIMIT 1) AS c_402,
    ((LENGTH(t_ref_399.id)::BOOLEAN) > NULLIF(('chaos' IN (SELECT sub_403.ref_tbl_03_id FROM tbl_04 AS sub_403)), 1)) AS c_405
FROM tbl_02 AS t_ref_399 LEFT JOIN tbl_06 AS j_ref_406 ON t_ref_399.id = j_ref_406.id
WHERE (t_ref_399.col_2 IN (SELECT sub_407.id FROM tbl_03 AS sub_407))
LIMIT 31;

CREATE VIEW v_summary_4 AS SELECT id FROM dev_tasks WHERE id IS NOT NULL;

UPDATE dev_tasks SET payload = 'escaped''quote' WHERE id > 29;

CREATE INDEX idx_tbl_07_id_6 ON tbl_07 (id);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4, ref_tbl_03_id, dyn_col_7) VALUES (180943, '{}', TRUE, '{}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 9223372036854775807, '__fuzz_marker__');

INSERT INTO tbl_09 (id, col_1, col_2) VALUES (972022, '[]', FALSE);

INSERT INTO tbl_10 (id, col_1, col_2) VALUES (729154, 1.0, '[]');

SELECT
    (((LENGTH((SELECT (SELECT inn_420.id FROM tbl_03 AS inn_420 LIMIT 1) FROM tbl_09 AS sub_418 LIMIT 1))::TEXT) + ((TRUE IN (SELECT sub_421.id FROM tbl_10 AS sub_421))::TEXT)) >= EXISTS (SELECT 1 FROM tbl_06 AS sub_422 WHERE FALSE)) AS c_425
FROM tbl_02 AS t_ref_417
WHERE t_ref_417.col_3
LIMIT 27;

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSONB,
    col_3 BOOLEAN,
    col_4 BOOLEAN,
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

ALTER TABLE tbl_05 DROP COLUMN col_1;

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_08 WHERE id IS NOT NULL;

SELECT
    (TRUE = -1) AS c_427,
    t_ref_426.payload AS c_428,
    (t_ref_426.payload::TEXT) AS c_429
FROM dev_tasks AS t_ref_426 LEFT JOIN tbl_09 AS j_ref_430 ON t_ref_426.id = j_ref_430.id
WHERE EXISTS (SELECT 1 FROM tbl_03 AS sub_431 WHERE FALSE)
LIMIT 34;

SELECT
    ((100::INT) > t_ref_432.col_3) AS c_433,
    NULLIF(((-9223372036854775808 AND ((SELECT ((CASE WHEN (SELECT (SELECT inn_438.col_2 FROM tbl_01 AS inn_438 LIMIT 1) FROM dev_tasks AS sub_435 LIMIT 1) THEN ((SELECT (SELECT inn_441.col_1 FROM tbl_08 AS inn_441 LIMIT 1) FROM tbl_11 AS sub_439 LIMIT 1)::TEXT::VARCHAR(32)) ELSE ((SELECT (sub_442.dyn_col_6 IN (SELECT sub_443.col_3 FROM tbl_08 AS sub_443)) FROM tbl_01 AS sub_442 LIMIT 1) / NULLIF(EXISTS (SELECT 1 FROM tbl_03 AS sub_446 WHERE sub_446.id), 0)) END) <= ABS(sub_434.id)) FROM tbl_05 AS sub_434 LIMIT 1) BETWEEN 100 AND 42)) > TRUE), (((SELECT LENGTH((((NOT (((((((CASE WHEN 'ref_val' THEN sub_449.col_1 ELSE sub_449.col_4 END) IS NULL) + ((sub_449.id < FALSE) < (FALSE::INT))) + (((sub_449.col_4::INT::TEXT) >= (TRUE::NUMERIC(10,2))) BETWEEN -9223372036854775808 AND -9223372036854775808))::INT::TEXT) - ((SELECT (SELECT inn_451.payload FROM dev_tasks AS inn_451 LIMIT 1) FROM tbl_10 AS sub_450 LIMIT 1) / NULLIF((CASE WHEN (sub_449.id IN (SELECT sub_452.payload FROM dev_tasks AS sub_452)) THEN (999999999.99 <> LENGTH(LENGTH(sub_449.id))) ELSE ((NULL * '__fuzz_marker__') >= TRUE) END), 0))))) - (SELECT LOWER(((((-9223372036854775808::TEXT::VARCHAR(32))::INT) - (SELECT sub_454.col_3 FROM tbl_11 AS sub_454 LIMIT 1)) BETWEEN 9223372036854775807 AND 0)) FROM dev_tasks AS sub_453 LIMIT 1))::TEXT::VARCHAR(32))) FROM tbl_08 AS sub_449 LIMIT 1) - (t_ref_432.dyn_col_7 / NULLIF(t_ref_432.col_1, 0))) OR ((SELECT (SELECT inn_468.col_2 FROM tbl_10 AS inn_468 LIMIT 1) FROM tbl_01 AS sub_455 LIMIT 1)::TEXT))) AS c_469
FROM tbl_04 AS t_ref_432
WHERE (((CASE WHEN ((TRUE::BOOLEAN) - (((NOT (((-2147483648 IN (SELECT sub_470.id FROM dev_tasks AS sub_470)) - (t_ref_432.dyn_col_7 OR (UPPER((((SELECT (SELECT inn_475.col_2 FROM tbl_10 AS inn_475 LIMIT 1) FROM tbl_10 AS sub_474 LIMIT 1)::NUMERIC(10,2)) OR 'escaped''quote'))::TEXT))))) * (t_ref_432.col_2 > ((UPPER(t_ref_432.col_1) BETWEEN 1 AND 42) - ((COALESCE(('__fuzz_marker__' IN (SELECT sub_476.col_2 FROM tbl_03 AS sub_476)), NULL) - (((NOT ((EXISTS (SELECT 1 FROM tbl_08 AS sub_477 WHERE 100) / NULLIF(-1e-5, 0)))) AND COALESCE((SELECT (SELECT FALSE FROM tbl_08 AS inn_479 LIMIT 1) FROM tbl_05 AS sub_478 LIMIT 1), FALSE)) <= ((FALSE IN (SELECT sub_480.col_2 FROM tbl_07 AS sub_480)) / NULLIF((((NOT (NULL)) / NULLIF(('chaos' - t_ref_432.col_3), 0)) <= (SELECT (SELECT inn_482.col_4 FROM tbl_11 AS inn_482 LIMIT 1) FROM tbl_07 AS sub_481 LIMIT 1)), 0)))) + (((((SELECT NULL FROM tbl_06 AS sub_483 LIMIT 1) AND t_ref_432.id)::TEXT) * (EXISTS (SELECT 1 FROM tbl_03 AS sub_484 WHERE sub_484.col_2) * EXISTS (SELECT 1 FROM dev_tasks AS sub_485 WHERE sub_485.payload))) / NULLIF(t_ref_432.col_1, 0))))))::TEXT::VARCHAR(32))) THEN (t_ref_432.dyn_col_7 IN (SELECT sub_487.payload FROM dev_tasks AS sub_487)) ELSE (LOWER(((((t_ref_432.col_4::BOOLEAN) + TRUE) - COALESCE((t_ref_432.dyn_col_7 IN (SELECT sub_488.col_2 FROM tbl_06 AS sub_488)), COALESCE(t_ref_432.id, (NOT (t_ref_432.dyn_col_7))))) AND ((TRUE * (((CASE WHEN (SELECT (SELECT inn_492.col_3 FROM tbl_02 AS inn_492 LIMIT 1) FROM tbl_10 AS sub_491 LIMIT 1) THEN LOWER((CASE WHEN (FALSE - 3.14159265) THEN NULLIF((-1.5 - t_ref_432.id), t_ref_432.col_1) ELSE ((t_ref_432.id * t_ref_432.dyn_col_7) / NULLIF('', 0)) END)) ELSE ((CASE WHEN (ABS('escaped''quote') < (NULL * t_ref_432.col_1)) THEN EXISTS (SELECT 1 FROM tbl_11 AS sub_493 WHERE sub_493.col_3) ELSE (CASE WHEN (t_ref_432.col_2 + t_ref_432.col_3) THEN (t_ref_432.id = t_ref_432.ref_tbl_03_id) ELSE t_ref_432.col_4 END) END)::TEXT::VARCHAR(32)) END) / NULLIF((SELECT LENGTH(ABS(COALESCE(sub_494.col_3, sub_494.id))) FROM tbl_08 AS sub_494 LIMIT 1), 0)) <> ((SELECT (SELECT 1 FROM tbl_06 AS inn_498 LIMIT 1) FROM tbl_09 AS sub_495 LIMIT 1) / NULLIF((((EXISTS (SELECT 1 FROM tbl_08 AS sub_499 WHERE 0.0000000001) IS NOT NULL) * -1) IS NOT NULL), 0)))) + (SELECT (SELECT inn_501.col_1 FROM tbl_04 AS inn_501 LIMIT 1) FROM tbl_09 AS sub_500 LIMIT 1)))) * EXISTS (SELECT 1 FROM tbl_09 AS sub_502 WHERE sub_502.col_2)) END) / NULLIF((t_ref_432.col_3 / NULLIF((SELECT ((ABS((sub_509.ref_tbl_02_id IN (SELECT sub_510.id FROM tbl_05 AS sub_510)))::BOOLEAN) < ((SELECT (SELECT inn_515.dyn_col_7 FROM tbl_04 AS inn_515 LIMIT 1) FROM tbl_03 AS sub_511 LIMIT 1) IS NOT NULL)) FROM tbl_06 AS sub_509 LIMIT 1), 0)), 0)) / NULLIF((SELECT sub_516.col_2 FROM tbl_06 AS sub_516 LIMIT 1), 0))
LIMIT 16;

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSONB
);

CREATE INDEX idx_tbl_02_col_2_7 ON tbl_02 (col_2);

ALTER TABLE tbl_01 DROP COLUMN col_3;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (991305, 1.0, 9223372036854775807, 'escaped''quote', '{}');

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2)
);

CREATE VIEW v_summary_6 AS SELECT id FROM tbl_06 WHERE id IS NOT NULL;

UPDATE tbl_09 SET col_2 = FALSE WHERE id > 95;

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (223437, TRUE, 'alpha_beta_gamma', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

CREATE VIEW v_summary_7 AS SELECT id FROM tbl_05 WHERE id IS NOT NULL;

SELECT
    (SELECT ((SELECT (SELECT NULL FROM tbl_11 AS inn_521 LIMIT 1) FROM tbl_04 AS sub_519 LIMIT 1) <> (SELECT (SELECT inn_529.col_1 FROM tbl_01 AS inn_529 LIMIT 1) FROM dev_tasks AS sub_522 LIMIT 1)) FROM tbl_06 AS sub_518 LIMIT 1) AS c_530,
    (SELECT ((1e10 AND ((NOT ((ABS(LOWER(COALESCE((SELECT (SELECT inn_536.col_1 FROM tbl_12 AS inn_536 LIMIT 1) FROM tbl_05 AS sub_532 LIMIT 1), (NULLIF((SELECT (sub_537.col_1::NUMERIC(10,2)) FROM tbl_12 AS sub_537 LIMIT 1), ((NULLIF(sub_531.col_1, FALSE) * (CASE WHEN 100 THEN sub_531.id ELSE sub_531.id END)) OR ((SELECT sub_538.id FROM tbl_12 AS sub_538 LIMIT 1)::NUMERIC(10,2)))) OR sub_531.id)))) / NULLIF(UPPER(sub_531.col_1), 0))))::TEXT::VARCHAR(32))) <= sub_531.col_2) FROM tbl_02 AS sub_531 LIMIT 1) AS c_539,
    (NOT (LENGTH((NULLIF(((t_ref_517.id OR ((ABS('chaos') + (TRUE IN (SELECT sub_540.col_2 FROM tbl_05 AS sub_540))) / NULLIF(t_ref_517.col_2, 0))) OR -9223372036854775808), (ABS((((SELECT (sub_541.col_2 IN (SELECT sub_542.col_1 FROM tbl_12 AS sub_542)) FROM tbl_11 AS sub_541 LIMIT 1) <= ((SELECT (COALESCE((NULL BETWEEN 42 AND -2147483648), (NOT (COALESCE(FALSE, sub_543.col_2)))) / NULLIF((((0.0 <= sub_543.id) / NULLIF((sub_543.col_2 * FALSE), 0)) >= -1e-5), 0)) FROM tbl_08 AS sub_543 LIMIT 1) - 100)) - t_ref_517.id)) * ((CASE WHEN ((((t_ref_517.id::INT::TEXT)::INT)::BOOLEAN) + TRUE) THEN ((((SELECT (NULLIF((SELECT (SELECT inn_546.id FROM tbl_01 AS inn_546 LIMIT 1) FROM tbl_03 AS sub_545 LIMIT 1), (sub_544.id - sub_544.col_2))::INT::TEXT) FROM tbl_05 AS sub_544 LIMIT 1) + NULL)::INT) - (SELECT (SELECT FALSE FROM tbl_11 AS inn_548 LIMIT 1) FROM tbl_11 AS sub_547 LIMIT 1)) ELSE (t_ref_517.id + (TRUE - (t_ref_517.id - ((NOT (EXISTS (SELECT 1 FROM tbl_07 AS sub_549 WHERE sub_549.col_1)))::TEXT::VARCHAR(32))))) END) / NULLIF(NULLIF(LENGTH(t_ref_517.col_2), TRUE), 0)))) - NULL)))) AS c_551
FROM tbl_05 AS t_ref_517 LEFT JOIN tbl_13 AS j_ref_552 ON t_ref_517.id = j_ref_552.id
WHERE (t_ref_517.col_2 IN (SELECT sub_553.col_2 FROM tbl_09 AS sub_553))
LIMIT 47;

SELECT
    ((COALESCE((((t_ref_554.ref_tbl_05_id - (t_ref_554.ref_tbl_05_id IN (SELECT sub_555.col_2 FROM tbl_10 AS sub_555))) IS NULL) OR (SELECT (SELECT inn_559.col_4 FROM tbl_02 AS inn_559 LIMIT 1) FROM tbl_05 AS sub_558 LIMIT 1)), (((UPPER(EXISTS (SELECT 1 FROM tbl_04 AS sub_560 WHERE sub_560.col_4))::TEXT::VARCHAR(32))::TEXT::VARCHAR(32))::TEXT::VARCHAR(32)))::TEXT) IS NULL) AS c_562
FROM tbl_07 AS t_ref_554 LEFT JOIN tbl_12 AS j_ref_563 ON t_ref_554.id = j_ref_563.id
WHERE ((LOWER(ABS(NULLIF((NULLIF((EXISTS (SELECT 1 FROM tbl_02 AS sub_564 WHERE sub_564.col_2) BETWEEN -1 AND 9223372036854775807), ('chaos' IN (SELECT sub_565.col_2 FROM tbl_08 AS sub_565))) / NULLIF(((SELECT (NULLIF((SELECT sub_569.id FROM tbl_10 AS sub_569 LIMIT 1), (NULLIF((FALSE::INT), ((sub_568.id <> sub_568.col_1) > sub_568.id)) * (EXISTS (SELECT 1 FROM tbl_08 AS sub_570 WHERE sub_570.col_3) OR (CASE WHEN (sub_568.col_2::INT::TEXT) THEN LENGTH(NULL) ELSE sub_568.col_1 END)))) = FALSE) FROM tbl_03 AS sub_568 LIMIT 1) * ((((SELECT (SELECT inn_574.col_2 FROM tbl_05 AS inn_574 LIMIT 1) FROM tbl_06 AS sub_571 LIMIT 1) <= t_ref_554.ref_tbl_05_id) > ((EXISTS (SELECT 1 FROM tbl_13 AS sub_575 WHERE 0.0) BETWEEN 42 AND 9223372036854775807) <= ((LENGTH(EXISTS (SELECT 1 FROM tbl_03 AS sub_576 WHERE sub_576.col_2)) <> EXISTS (SELECT 1 FROM tbl_11 AS sub_577 WHERE sub_577.col_2)) * (((CASE WHEN t_ref_554.col_2 THEN (t_ref_554.ref_tbl_05_id AND TRUE) ELSE LOWER(t_ref_554.col_3) END) OR (LENGTH(TRUE) < (t_ref_554.ref_tbl_05_id + t_ref_554.col_2))) AND (UPPER(-2147483648) OR ((FALSE > t_ref_554.id)::NUMERIC(10,2))))))) / NULLIF(ABS(COALESCE((SELECT EXISTS (SELECT 1 FROM tbl_13 AS sub_579 WHERE sub_579.col_1) FROM tbl_04 AS sub_578 LIMIT 1), (((SELECT (SELECT inn_581.col_2 FROM tbl_03 AS inn_581 LIMIT 1) FROM dev_tasks AS sub_580 LIMIT 1) <= t_ref_554.col_2) OR t_ref_554.col_3))), 0))), 0)), (t_ref_554.ref_tbl_05_id + (SELECT (SELECT inn_584.col_1 FROM tbl_03 AS inn_584 LIMIT 1) FROM tbl_08 AS sub_582 LIMIT 1))))) - (CASE WHEN LOWER(t_ref_554.col_1) THEN (SELECT (SELECT 'chaos' FROM tbl_08 AS inn_592 LIMIT 1) FROM tbl_13 AS sub_585 LIMIT 1) ELSE ((SELECT (SELECT inn_594.col_1 FROM tbl_06 AS inn_594 LIMIT 1) FROM tbl_04 AS sub_593 LIMIT 1)::INT) END)) * EXISTS (SELECT 1 FROM tbl_09 AS sub_595 WHERE sub_595.col_2))
LIMIT 33;

DELETE FROM tbl_02 WHERE id = 45028;

SELECT
    ((SELECT (SELECT inn_602.payload FROM dev_tasks AS inn_602 LIMIT 1) FROM tbl_01 AS sub_600 LIMIT 1) > (CASE WHEN t_ref_599.id THEN (1.0 < t_ref_599.id) ELSE ((COALESCE(EXISTS (SELECT 1 FROM tbl_03 AS sub_603 WHERE sub_603.col_2), (CASE WHEN (CASE WHEN (EXISTS (SELECT 1 FROM tbl_01 AS sub_604 WHERE sub_604.col_4)::INT::TEXT) THEN (t_ref_599.id AND (SELECT (SELECT inn_610.col_2 FROM tbl_06 AS inn_610 LIMIT 1) FROM tbl_09 AS sub_606 LIMIT 1)) ELSE EXISTS (SELECT 1 FROM tbl_02 AS sub_611 WHERE sub_611.col_2) END) THEN ((t_ref_599.id = t_ref_599.col_2) + (t_ref_599.id IN (SELECT sub_615.ref_tbl_05_id FROM tbl_07 AS sub_615))) ELSE (CASE WHEN (t_ref_599.col_2::TEXT::VARCHAR(32)) THEN 'alpha_beta_gamma' ELSE (NOT ((CASE WHEN EXISTS (SELECT 1 FROM tbl_13 AS sub_616 WHERE sub_616.col_1) THEN ((LENGTH((CASE WHEN (SELECT (SELECT TRUE FROM tbl_03 AS inn_621 LIMIT 1) FROM dev_tasks AS sub_620 LIMIT 1) THEN LOWER((NOT (t_ref_599.col_1))) ELSE (SELECT (SELECT inn_623.col_1 FROM tbl_10 AS inn_623 LIMIT 1) FROM tbl_12 AS sub_622 LIMIT 1) END))::TEXT::VARCHAR(32)) >= LENGTH((CASE WHEN FALSE THEN (t_ref_599.col_2 IN (SELECT sub_624.col_2 FROM tbl_12 AS sub_624)) ELSE COALESCE(t_ref_599.col_1, UPPER((t_ref_599.id + t_ref_599.id))) END))) ELSE (NULL * EXISTS (SELECT 1 FROM dev_tasks AS sub_625 WHERE 2147483647)) END))) END) END)) / NULLIF(('chaos' AND ((1::INT) - ((((t_ref_599.col_2 / NULLIF((((42 + (t_ref_599.col_1 IN (SELECT sub_626.col_3 FROM tbl_04 AS sub_626))) * (UPPER((NOT (TRUE))) / NULLIF(t_ref_599.col_1, 0))) >= ((-1e-5 / NULLIF(((SELECT sub_627.col_1 FROM tbl_03 AS sub_627 LIMIT 1) <> (t_ref_599.col_2 <> t_ref_599.col_1)), 0)) IS NULL)), 0)) BETWEEN 0 AND 2147483647) IS NULL)::INT::TEXT))), 0)) OR t_ref_599.id) END)) AS c_628,
    1 AS c_629,
    ((t_ref_599.col_1 - ((((((ABS(t_ref_599.col_1)::TEXT::VARCHAR(32)) * LOWER((((-1 * (t_ref_599.col_2 / NULLIF((((t_ref_599.col_1 BETWEEN 1 AND 1)::BOOLEAN) - LOWER('chaos')), 0))) / NULLIF(COALESCE(t_ref_599.id, EXISTS (SELECT 1 FROM tbl_06 AS sub_630 WHERE sub_630.col_1)), 0)) - (SELECT 1 FROM tbl_13 AS sub_631 LIMIT 1)))) AND t_ref_599.id) IS NOT NULL)::INT::TEXT) AND ((-2147483648 IN (SELECT sub_632.col_1 FROM tbl_09 AS sub_632)) > (((SELECT ((FALSE >= (sub_636.col_1 IN (SELECT sub_637.col_1 FROM tbl_01 AS sub_637))) <= sub_636.col_2) FROM tbl_13 AS sub_636 LIMIT 1) / NULLIF(((SELECT (sub_639.id * ((CASE WHEN UPPER((CASE WHEN (sub_639.col_2 IN (SELECT sub_640.id FROM tbl_06 AS sub_640)) THEN NULLIF(0.0, sub_639.id) ELSE (TRUE::BOOLEAN) END)) THEN sub_639.col_2 ELSE (EXISTS (SELECT 1 FROM tbl_09 AS sub_641 WHERE 1.0) BETWEEN -1 AND -2147483648) END)::INT::TEXT)) FROM tbl_05 AS sub_639 LIMIT 1) * ((t_ref_599.col_2 > (NOT ((SELECT sub_642.col_4 FROM tbl_02 AS sub_642 LIMIT 1)))) AND (((t_ref_599.col_2 IN (SELECT sub_643.col_1 FROM tbl_12 AS sub_643))::TEXT) BETWEEN 9223372036854775807 AND -1))), 0))::NUMERIC(10,2)))))::TEXT) AS c_644
FROM tbl_10 AS t_ref_599
WHERE 1
LIMIT 32;

SELECT
    (EXISTS (SELECT 1 FROM tbl_08 AS sub_646 WHERE NULL) < (COALESCE(EXISTS (SELECT 1 FROM dev_tasks AS sub_649 WHERE '__fuzz_marker__'), ((t_ref_645.col_1 IN (SELECT sub_650.id FROM tbl_05 AS sub_650)) = FALSE))::INT)) AS c_651
FROM tbl_13 AS t_ref_645
WHERE ((CASE WHEN EXISTS (SELECT 1 FROM tbl_02 AS sub_652 WHERE sub_652.col_4) THEN t_ref_645.col_2 ELSE LENGTH((((((SELECT (SELECT inn_655.col_2 FROM tbl_09 AS inn_655 LIMIT 1) FROM dev_tasks AS sub_653 LIMIT 1) OR ((t_ref_645.col_1 - (UPPER(COALESCE(((t_ref_645.col_1 >= (t_ref_645.col_2 / NULLIF(t_ref_645.id, 0))) - (t_ref_645.col_1 >= ('escaped''quote'::TEXT::VARCHAR(32)))), (NULL::TEXT::VARCHAR(32)))) - (SELECT (SELECT inn_657.col_2 FROM tbl_01 AS inn_657 LIMIT 1) FROM tbl_03 AS sub_656 LIMIT 1))) AND (NULLIF(((NULLIF(((SELECT sub_658.col_2 FROM tbl_11 AS sub_658 LIMIT 1) + EXISTS (SELECT 1 FROM tbl_03 AS sub_659 WHERE sub_659.col_2)), (LENGTH(t_ref_645.col_2) BETWEEN 42 AND -9223372036854775808))::INT) <= (SELECT (SELECT 3.14159265 FROM dev_tasks AS inn_663 LIMIT 1) FROM tbl_04 AS sub_660 LIMIT 1)), ABS((CASE WHEN (SELECT (SELECT inn_665.col_1 FROM tbl_10 AS inn_665 LIMIT 1) FROM tbl_05 AS sub_664 LIMIT 1) THEN (ABS((t_ref_645.id OR FALSE)) < UPPER((NULL BETWEEN 1 AND -9223372036854775808))) ELSE (FALSE * (t_ref_645.col_1 / NULLIF(0.0000000001, 0))) END))) * (((((NOT ((t_ref_645.id * t_ref_645.col_2))) BETWEEN 1 AND 1) IS NOT NULL) IS NULL) - LOWER(((t_ref_645.col_2 IN (SELECT sub_666.ref_tbl_02_id FROM tbl_11 AS sub_666)) * (((t_ref_645.col_1 AND t_ref_645.col_2) * (SELECT (SELECT inn_669.col_1 FROM tbl_13 AS inn_669 LIMIT 1) FROM tbl_08 AS sub_668 LIMIT 1))::BOOLEAN))))))) - TRUE) = (SELECT (SELECT 1 FROM tbl_04 AS inn_671 LIMIT 1) FROM tbl_13 AS sub_670 LIMIT 1)) / NULLIF(UPPER(t_ref_645.col_2), 0))) END) BETWEEN -1 AND -2147483648)
LIMIT 37;

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 JSONB
);

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64),
    col_4 INT,
    ref_tbl_09_id INT,
    FOREIGN KEY (ref_tbl_09_id) REFERENCES tbl_09(id)
);

UPDATE tbl_01 SET col_4 = '{}' WHERE id > 43;

UPDATE tbl_09 SET col_1 = '{}' WHERE id > 48;

ALTER TABLE tbl_01 DROP COLUMN dyn_col_6;

SELECT
    ((((SELECT LOWER(((CASE WHEN ((SELECT (SELECT inn_675.id FROM tbl_13 AS inn_675 LIMIT 1) FROM tbl_12 AS sub_674 LIMIT 1) + sub_673.id) THEN -2147483648 ELSE sub_673.id END) + (sub_673.id - COALESCE(sub_673.col_1, sub_673.col_1)))) FROM tbl_09 AS sub_673 LIMIT 1) / NULLIF(NULLIF((999999999.99 IN (SELECT sub_676.col_1 FROM tbl_01 AS sub_676)), LENGTH(ABS((FALSE - ((((t_ref_672.id / NULLIF((EXISTS (SELECT 1 FROM tbl_06 AS sub_686 WHERE TRUE) - t_ref_672.col_1), 0)) + (SELECT (SELECT inn_688.id FROM dev_tasks AS inn_688 LIMIT 1) FROM tbl_01 AS sub_687 LIMIT 1)) BETWEEN -2147483648 AND 2147483647) >= t_ref_672.col_4))))), 0))::INT::TEXT) OR t_ref_672.id) AS c_689,
    ABS((FALSE::TEXT)) AS c_690
FROM tbl_01 AS t_ref_672
WHERE (999999999.99 IN (SELECT sub_691.col_4 FROM tbl_11 AS sub_691))
LIMIT 49;

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 BOOLEAN,
    ref_tbl_07_id INT,
    FOREIGN KEY (ref_tbl_07_id) REFERENCES tbl_07(id)
);

CREATE VIEW v_summary_8 AS SELECT id FROM tbl_16 WHERE id IS NOT NULL;

INSERT INTO tbl_13 (id, col_1, col_2) VALUES (302813, 'escaped''quote', 3.14159265);

ALTER TABLE tbl_16 DROP COLUMN ref_tbl_07_id;

INSERT INTO tbl_06 (id, col_1, col_2, ref_tbl_02_id) VALUES (30058, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'alpha_beta_gamma', -1);

CREATE TABLE tbl_17 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2),
    col_4 BOOLEAN,
    ref_tbl_07_id INT,
    FOREIGN KEY (ref_tbl_07_id) REFERENCES tbl_07(id)
);

CREATE VIEW v_summary_9 AS SELECT id FROM tbl_17 WHERE id IS NOT NULL;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (987671, 1e10, 9223372036854775807, 'alpha_beta_gamma', '[]');

SELECT
    (t_ref_705.id AND LOWER(t_ref_705.ref_tbl_03_id)) AS c_706
FROM tbl_04 AS t_ref_705 LEFT JOIN tbl_03 AS j_ref_707 ON t_ref_705.id = j_ref_707.id
WHERE ((t_ref_705.col_2 IN (SELECT sub_708.col_2 FROM tbl_01 AS sub_708))::TEXT)
LIMIT 3;

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (288530, FALSE, 'chaos', '{}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

INSERT INTO tbl_05 (id, col_2) VALUES (269586, '__fuzz_marker__');

CREATE TABLE tbl_18 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 BOOLEAN,
    col_4 VARCHAR(64),
    ref_tbl_05_id INT,
    FOREIGN KEY (ref_tbl_05_id) REFERENCES tbl_05(id)
);

CREATE TYPE task_priority AS ENUM ('low', 'medium', 'high', 'critical');

SELECT
    t.task_id,
    t.title,
    t.priority,
    (
        SELECT COUNT(*)
        FROM unnest(enum_range(NULL::task_priority, t.priority)) AS r(p)
        WHERE r.p > 'low'::task_priority
    ) AS prior_count
FROM dev_tasks t
ORDER BY t.priority;

INSERT INTO tbl_03 (id, col_1, col_2) VALUES (95610, FALSE, FALSE);

SELECT
    TRUE AS c_717,
    NULLIF(42, ((LENGTH(((UPPER((t_ref_716.col_1 / NULLIF('escaped''quote', 0))) * ((SELECT (((((sub_718.col_4::TEXT::VARCHAR(32)) <= ((sub_718.col_2 * sub_718.col_3) < ('chaos' + 'alpha_beta_gamma'))) OR NULLIF(LENGTH(NULLIF(sub_718.col_3, sub_718.id)), (sub_718.id IN (SELECT sub_719.col_4 FROM tbl_02 AS sub_719)))) >= (NULLIF((SELECT sub_720.id FROM dev_tasks AS sub_720 LIMIT 1), ((sub_718.col_3 <= sub_718.col_3) / NULLIF((sub_718.col_3 / NULLIF(sub_718.col_2, 0)), 0))) = (((NOT (sub_718.col_2)) + (''::BOOLEAN)) < TRUE)))::TEXT) FROM tbl_18 AS sub_718 LIMIT 1) IS NULL))::TEXT::VARCHAR(32))) - 'ref_val')::TEXT::VARCHAR(32))) AS c_721,
    (t_ref_716.id BETWEEN 1 AND 9223372036854775807) AS c_722
FROM tbl_09 AS t_ref_716 LEFT JOIN dev_tasks AS j_ref_723 ON t_ref_716.id = j_ref_723.id
WHERE FALSE
LIMIT 23;

INSERT INTO tbl_06 (id, col_1, col_2, ref_tbl_02_id) VALUES (14317, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'chaos', -2147483648);

UPDATE tbl_17 SET col_4 = TRUE WHERE id > 62;

INSERT INTO tbl_11 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (118587, 1e10, '[]', TRUE, TRUE, -9223372036854775808);

UPDATE tbl_05 SET col_2 = 'ref_val' WHERE id > 67;

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, ref_tbl_02_id) VALUES (758426, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'chaos', 42);
COMMIT;

CREATE INDEX idx_tbl_07_col_3_8 ON tbl_07 (col_3);

UPDATE tbl_08 SET col_3 = '[]' WHERE id > 81;

INSERT INTO tbl_18 (id, col_1, col_2, col_3, col_4, ref_tbl_05_id) VALUES (607463, '', TRUE, TRUE, 'alpha_beta_gamma', 1);

INSERT INTO tbl_16 (id, col_1, col_2) VALUES (596044, '{}', TRUE);

INSERT INTO tbl_05 (id, col_2) VALUES (573957, '__fuzz_marker__');

INSERT INTO tbl_11 (id, col_1, col_2, col_3, col_4, ref_tbl_02_id) VALUES (564833, 0.0000000001, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', FALSE, FALSE, 2147483647);

BEGIN;
INSERT INTO tbl_13 (id, col_1, col_2) VALUES (294565, '', -1.5);
INSERT INTO tbl_10 (id, col_1, col_2) VALUES (98559, 999999999.99, '[]');
COMMIT;

ALTER TABLE tbl_01 ADD COLUMN dyn_col_5 VARCHAR(32);

BEGIN;
INSERT INTO tbl_17 (id, col_1, col_2, col_3, col_4, ref_tbl_07_id) VALUES (175721, 1.0, 'chaos', 1.0, TRUE, 1);
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4) VALUES (32112, 1e10, 9223372036854775807, 'alpha_beta_gamma', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
INSERT INTO tbl_09 (id, col_1, col_2) VALUES (873810, '[]', TRUE);
COMMIT;

CREATE TABLE tbl_19 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 INT,
    col_3 BOOLEAN,
    col_4 JSONB
);

CREATE VIEW v_summary_10 AS SELECT id FROM tbl_16 WHERE id IS NOT NULL;

CREATE INDEX idx_dev_tasks_payload_9 ON dev_tasks (payload);

UPDATE tbl_03 SET col_2 = FALSE WHERE id > 94;

UPDATE tbl_17 SET col_2 = 'escaped''quote' WHERE id > 12;

BEGIN;
INSERT INTO tbl_10 (id, col_1, col_2) VALUES (322407, 1.0, '{}');
COMMIT;

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (324824, FALSE, 'alpha_beta_gamma', '[]', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_5) VALUES (489789, FALSE, 'ref_val', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '');
INSERT INTO tbl_01 (id, col_1, col_2, col_4, dyn_col_5) VALUES (587, FALSE, '__fuzz_marker__', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 'escaped''quote');
COMMIT;

CREATE VIEW v_summary_11 AS SELECT id FROM tbl_05 WHERE id IS NOT NULL;

CREATE TABLE tbl_20 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 INT
);

CREATE INDEX idx_tbl_04_ref_tbl_03_id_10 ON tbl_04 (ref_tbl_03_id);

SELECT
    (NULL::NUMERIC(10,2)) AS c_725,
    t_ref_724.id AS c_726
FROM tbl_05 AS t_ref_724 LEFT JOIN tbl_06 AS j_ref_727 ON t_ref_724.id = j_ref_727.id
WHERE (NOT (t_ref_724.id))
LIMIT 18;

INSERT INTO tbl_13 (id, col_1, col_2) VALUES (61498, '', 999999999.99);

BEGIN;
INSERT INTO tbl_12 (id, col_1, col_2) VALUES (865994, -9223372036854775808, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

SELECT
    (ABS('ref_val') * (((NOT (t_ref_728.col_4))::BOOLEAN) + NULLIF(TRUE, (((NOT (LOWER(UPPER((t_ref_728.col_1 < (((t_ref_728.col_3 IN (SELECT sub_729.id FROM tbl_16 AS sub_729)) * ((EXISTS (SELECT 1 FROM tbl_18 AS sub_730 WHERE sub_730.col_1) - (LENGTH(TRUE) * (SELECT (SELECT inn_732.col_4 FROM tbl_08 AS inn_732 LIMIT 1) FROM tbl_07 AS sub_731 LIMIT 1))) + (SELECT (SELECT inn_734.col_1 FROM tbl_02 AS inn_734 LIMIT 1) FROM tbl_07 AS sub_733 LIMIT 1))) * t_ref_728.id)))))) - FALSE) > UPPER((t_ref_728.col_3::TEXT)))))) AS c_735,
    (SELECT (SELECT inn_737.col_1 FROM tbl_09 AS inn_737 LIMIT 1) FROM tbl_10 AS sub_736 LIMIT 1) AS c_738,
    t_ref_728.col_4 AS c_739
FROM tbl_02 AS t_ref_728 LEFT JOIN tbl_12 AS j_ref_740 ON t_ref_728.id = j_ref_740.id
WHERE LENGTH(t_ref_728.col_3)
LIMIT 48;
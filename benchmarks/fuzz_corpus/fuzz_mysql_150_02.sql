-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: MYSQL

-- Statements: 150

-- AST Expression Depth: 18

-- Injected Defect Case: mysql_08

-- Seed: 15022



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSON
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 BOOLEAN,
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON,
    col_3 VARCHAR(64),
    col_4 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN
);

CREATE TABLE invoices (
    invoice_id INT PRIMARY KEY,
    region_id INT NOT NULL,
    sales_rep_id INT NOT NULL,
    invoice_amount DECIMAL(10,2) NOT NULL
);

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 JSON
);

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (384842, '{}', '{}', 'alpha_beta_gamma', 'alpha_beta_gamma', 100);

SELECT
    NULL AS c_2,
    CAST((SELECT (NULLIF(CAST(NULLIF((SELECT (SELECT inn_15.col_1 FROM tbl_05 AS inn_15 LIMIT 1) FROM tbl_01 AS sub_4 LIMIT 1), ((CASE WHEN UPPER((NULLIF(LOWER(CAST(COALESCE(sub_3.id, ((sub_3.id < 'alpha_beta_gamma') + ('alpha_beta_gamma' BETWEEN 100 AND 1))) AS DECIMAL(10,2))), (UPPER((SELECT (SELECT 1e10 FROM invoices AS inn_19 LIMIT 1) FROM tbl_06 AS sub_16 LIMIT 1)) - sub_3.col_2)) <= LENGTH(sub_3.id))) THEN 9223372036854775807 ELSE sub_3.col_1 END) BETWEEN -2147483648 AND -1)) AS DECIMAL(10,2)), (sub_3.id IN (SELECT sub_20.id FROM tbl_06 AS sub_20))) < (SELECT (SELECT inn_25.col_2 FROM tbl_01 AS inn_25 LIMIT 1) FROM tbl_06 AS sub_24 LIMIT 1)) FROM tbl_01 AS sub_3 LIMIT 1) AS CHAR) AS c_26,
    LOWER(t_ref_1.id) AS c_27
FROM invoices AS t_ref_1
WHERE ((SELECT (CASE WHEN UPPER(EXISTS (SELECT 1 FROM tbl_06 AS sub_29 WHERE FALSE)) THEN ((sub_28.col_2 IN (SELECT sub_30.col_2 FROM tbl_01 AS sub_30)) - 0) ELSE EXISTS (SELECT 1 FROM tbl_03 AS sub_38 WHERE NULL) END) FROM tbl_01 AS sub_28 LIMIT 1) - t_ref_1.payload)
LIMIT 48;

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

INSERT INTO tbl_02 (id, col_1, col_2, ref_tbl_01_id) VALUES (719860, 1.0, TRUE, 2147483647);

UPDATE tbl_04 SET col_3 = 'ref_val' WHERE id > 67;

DELETE FROM tbl_06 WHERE id = 80244;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 VARCHAR(64)
);

SELECT
    t_ref_39.ref_tbl_01_id AS c_40,
    CAST(CAST((CAST(FALSE AS CHAR) + ((SELECT 100 FROM tbl_03 AS sub_41 LIMIT 1) / NULLIF(((((SELECT sub_42.id FROM tbl_05 AS sub_42 LIMIT 1) = (SELECT sub_43.col_2 FROM tbl_05 AS sub_43 LIMIT 1)) * (t_ref_39.ref_tbl_01_id IN (SELECT sub_44.id FROM tbl_06 AS sub_44))) * (((((SELECT (SELECT NULL FROM tbl_07 AS inn_55 LIMIT 1) FROM tbl_02 AS sub_54 LIMIT 1) < ((SELECT (SELECT inn_57.id FROM tbl_02 AS inn_57 LIMIT 1) FROM tbl_05 AS sub_56 LIMIT 1) - (SELECT sub_58.ref_tbl_01_id FROM tbl_02 AS sub_58 LIMIT 1))) <> UPPER(NULLIF((t_ref_39.ref_tbl_01_id <> (SELECT (SELECT inn_60.id FROM tbl_07 AS inn_60 LIMIT 1) FROM tbl_07 AS sub_59 LIMIT 1)), LOWER('__fuzz_marker__')))) / NULLIF(1, 0)) + UPPER((SELECT (SELECT 9223372036854775807 FROM tbl_06 AS inn_62 LIMIT 1) FROM tbl_07 AS sub_61 LIMIT 1)))), 0))) AS SIGNED) AS SIGNED) AS c_63,
    t_ref_39.col_2 AS c_64
FROM tbl_04 AS t_ref_39 LEFT JOIN tbl_01 AS j_ref_65 ON t_ref_39.id = j_ref_65.id
WHERE (CASE WHEN COALESCE((CASE WHEN t_ref_39.id THEN EXISTS (SELECT 1 FROM tbl_07 AS sub_66 WHERE sub_66.col_2) ELSE UPPER(((CASE WHEN (t_ref_39.id AND t_ref_39.col_4) THEN (t_ref_39.col_1 = (((CASE WHEN (SELECT ((((sub_67.col_1 >= FALSE) < CAST((sub_67.col_1 * sub_67.col_2) AS SIGNED)) IS NULL) BETWEEN 42 AND 100) FROM tbl_02 AS sub_67 LIMIT 1) THEN (0.0000000001 + (NULLIF(CAST((UPPER(FALSE) + NULL) AS DECIMAL(10,2)), t_ref_39.col_2) / NULLIF((NOT (1e10)), 0))) ELSE t_ref_39.col_1 END) + LOWER((EXISTS (SELECT 1 FROM tbl_06 AS sub_68 WHERE sub_68.col_2) / NULLIF((NOT ((((FALSE IN (SELECT sub_69.col_2 FROM tbl_06 AS sub_69)) * (2147483647 IN (SELECT sub_70.payload FROM invoices AS sub_70))) + t_ref_39.id))), 0)))) AND (SELECT (SELECT TRUE FROM tbl_05 AS inn_74 LIMIT 1) FROM invoices AS sub_71 LIMIT 1))) ELSE t_ref_39.col_1 END) / NULLIF(COALESCE('chaos', t_ref_39.col_3), 0))) END), ((t_ref_39.col_2 OR ((((LOWER(1e10) - (CASE WHEN (t_ref_39.col_2 IN (SELECT sub_75.id FROM invoices AS sub_75)) THEN FALSE ELSE (CASE WHEN CAST((t_ref_39.col_1 >= (CASE WHEN EXISTS (SELECT 1 FROM tbl_03 AS sub_76 WHERE sub_76.col_1) THEN CAST((((CASE WHEN NULL THEN t_ref_39.col_2 ELSE TRUE END) + t_ref_39.id) > (EXISTS (SELECT 1 FROM tbl_04 AS sub_79 WHERE sub_79.col_4) <> (t_ref_39.ref_tbl_01_id * t_ref_39.col_3))) AS DECIMAL(10,2)) ELSE (t_ref_39.col_1 BETWEEN 2147483647 AND 100) END)) AS CHAR) THEN t_ref_39.col_4 ELSE t_ref_39.col_4 END) END)) / NULLIF((COALESCE(CAST((CASE WHEN t_ref_39.id THEN (t_ref_39.col_4 IN (SELECT sub_80.col_4 FROM tbl_04 AS sub_80)) ELSE t_ref_39.col_2 END) AS DECIMAL(10,2)), ((LENGTH((t_ref_39.id IN (SELECT sub_81.ref_tbl_01_id FROM tbl_02 AS sub_81))) + (((SELECT COALESCE(CAST(sub_82.id AS DECIMAL(10,2)), (sub_82.ref_tbl_02_id + NULL)) FROM tbl_03 AS sub_82 LIMIT 1) + t_ref_39.ref_tbl_01_id) BETWEEN -9223372036854775808 AND 100)) BETWEEN -1 AND 9223372036854775807)) * UPPER((SELECT (((CASE WHEN 0.0000000001 THEN ((sub_83.id IN (SELECT sub_84.id FROM tbl_02 AS sub_84)) + sub_83.col_2) ELSE (SELECT (NOT (-9223372036854775808)) FROM tbl_05 AS sub_85 LIMIT 1) END) BETWEEN 2147483647 AND 9223372036854775807) IS NOT NULL) FROM tbl_01 AS sub_83 LIMIT 1))), 0)) / NULLIF(((CAST(CAST((FALSE - (t_ref_39.col_2 / NULLIF((EXISTS (SELECT 1 FROM tbl_06 AS sub_86 WHERE sub_86.col_2) OR t_ref_39.col_2), 0))) AS CHAR) AS SIGNED) - NULL) > 0.0), 0)) = (t_ref_39.col_1 IN (SELECT sub_88.col_1 FROM tbl_01 AS sub_88)))) - LOWER(((((UPPER((COALESCE(t_ref_39.id, COALESCE(((SELECT (SELECT -2147483648 FROM tbl_07 AS inn_94 LIMIT 1) FROM tbl_07 AS sub_93 LIMIT 1) IS NOT NULL), t_ref_39.id)) / NULLIF(t_ref_39.col_2, 0))) - CAST(CAST(NULLIF(((t_ref_39.col_1 IN (SELECT sub_95.col_2 FROM tbl_07 AS sub_95)) + t_ref_39.col_4), (CASE WHEN t_ref_39.ref_tbl_01_id THEN t_ref_39.col_1 ELSE (FALSE BETWEEN 0 AND 2147483647) END)) AS CHAR) AS DECIMAL(10,2))) > ((((((CASE WHEN t_ref_39.col_1 THEN (SELECT (SELECT NULL FROM tbl_05 AS inn_97 LIMIT 1) FROM tbl_01 AS sub_96 LIMIT 1) ELSE ((SELECT (SELECT FALSE FROM tbl_06 AS inn_99 LIMIT 1) FROM tbl_06 AS sub_98 LIMIT 1) >= t_ref_39.id) END) + ((SELECT ((sub_100.col_1 > sub_100.id) * 999999999.99) FROM tbl_02 AS sub_100 LIMIT 1) / NULLIF(((SELECT sub_101.col_1 FROM tbl_01 AS sub_101 LIMIT 1) * CAST((CASE WHEN (CASE WHEN t_ref_39.col_3 THEN t_ref_39.ref_tbl_01_id ELSE t_ref_39.col_4 END) THEN UPPER(t_ref_39.id) ELSE (NULL <= t_ref_39.col_1) END) AS CHAR)), 0))) AND ((1e10 / NULLIF((CAST(CAST((CASE WHEN t_ref_39.col_1 THEN t_ref_39.ref_tbl_01_id ELSE t_ref_39.col_1 END) AS DECIMAL(10,2)) AS CHAR) + (CASE WHEN -1e-5 THEN 'ref_val' ELSE ((CASE WHEN t_ref_39.col_2 THEN t_ref_39.col_2 ELSE t_ref_39.id END) <= (t_ref_39.col_4 AND t_ref_39.ref_tbl_01_id)) END)), 0)) / NULLIF(UPPER(ABS(('chaos' BETWEEN 0 AND 42))), 0))) - (t_ref_39.id - (NOT (-2147483648)))) >= ((NOT (t_ref_39.ref_tbl_01_id)) = NULLIF(CAST(CAST((NOT (FALSE)) AS SIGNED) AS SIGNED), ''))) + CAST(((SELECT sub_102.col_2 FROM tbl_04 AS sub_102 LIMIT 1) <> -2147483648) AS DECIMAL(10,2)))) + t_ref_39.col_2) * (LOWER(((((CAST((SELECT (SELECT 100 FROM invoices AS inn_105 LIMIT 1) FROM tbl_01 AS sub_103 LIMIT 1) AS CHAR) < t_ref_39.col_1) >= EXISTS (SELECT 1 FROM tbl_01 AS sub_106 WHERE sub_106.col_2)) + UPPER(CAST(((CAST(NULLIF(((t_ref_39.col_1 + t_ref_39.id) BETWEEN -9223372036854775808 AND 2147483647), t_ref_39.id) AS CHAR) + EXISTS (SELECT 1 FROM tbl_05 AS sub_107 WHERE sub_107.col_2)) >= CAST((FALSE / NULLIF((t_ref_39.ref_tbl_01_id IN (SELECT sub_109.id FROM invoices AS sub_109)), 0)) AS SIGNED)) AS DECIMAL(10,2)))) < (SELECT (SELECT 9223372036854775807 FROM tbl_06 AS inn_111 LIMIT 1) FROM tbl_01 AS sub_110 LIMIT 1))) <> (t_ref_39.col_3 + ((SELECT CAST(sub_112.col_2 AS DECIMAL(10,2)) FROM tbl_05 AS sub_112 LIMIT 1) <> (NOT ((((('' IN (SELECT sub_113.id FROM tbl_01 AS sub_113)) = ((t_ref_39.col_4 BETWEEN 0 AND 9223372036854775807) > t_ref_39.ref_tbl_01_id)) * (t_ref_39.id IN (SELECT sub_115.col_1 FROM tbl_06 AS sub_115))) - CAST((SELECT (SELECT 0.0000000001 FROM invoices AS inn_119 LIMIT 1) FROM tbl_02 AS sub_117 LIMIT 1) AS SIGNED))))))))))) THEN ((ABS(((CASE WHEN NULLIF(t_ref_39.col_4, t_ref_39.id) THEN ((SELECT (SELECT inn_123.col_3 FROM tbl_06 AS inn_123 LIMIT 1) FROM tbl_01 AS sub_120 LIMIT 1) >= NULL) ELSE (((SELECT (SELECT '__fuzz_marker__' FROM tbl_05 AS inn_127 LIMIT 1) FROM tbl_02 AS sub_124 LIMIT 1) / NULLIF((CASE WHEN (((SELECT (SELECT inn_129.col_1 FROM tbl_05 AS inn_129 LIMIT 1) FROM tbl_04 AS sub_128 LIMIT 1) + t_ref_39.id) IS NULL) THEN (CAST((CAST(((((t_ref_39.ref_tbl_01_id + t_ref_39.col_2) = t_ref_39.col_1) + (t_ref_39.ref_tbl_01_id + (t_ref_39.col_2 - t_ref_39.col_1))) OR (CASE WHEN (t_ref_39.col_2 IN (SELECT sub_130.col_1 FROM tbl_02 AS sub_130)) THEN 100 ELSE CAST(CAST(t_ref_39.id AS CHAR) AS DECIMAL(10,2)) END)) AS SIGNED) * t_ref_39.col_2) AS SIGNED) / NULLIF((LOWER((SELECT (SELECT inn_132.col_2 FROM tbl_05 AS inn_132 LIMIT 1) FROM tbl_04 AS sub_131 LIMIT 1)) BETWEEN 42 AND 1), 0)) ELSE (NOT ((NOT (((CAST(-1 AS CHAR) IS NOT NULL) <> t_ref_39.col_4))))) END), 0)) * 9223372036854775807) END) + t_ref_39.col_4)) * t_ref_39.ref_tbl_01_id) AND (SELECT (SELECT UPPER(sub_134.payload) FROM invoices AS sub_134 LIMIT 1) FROM tbl_04 AS sub_133 LIMIT 1)) ELSE ((((CASE WHEN FALSE THEN LOWER(t_ref_39.ref_tbl_01_id) ELSE (EXISTS (SELECT 1 FROM tbl_06 AS sub_135 WHERE FALSE) * (COALESCE((t_ref_39.col_3 < EXISTS (SELECT 1 FROM tbl_05 AS sub_137 WHERE -1.5)), 0.0000000001) >= t_ref_39.col_2)) END) >= (EXISTS (SELECT 1 FROM tbl_07 AS sub_139 WHERE 0.0) < (NOT (t_ref_39.col_4)))) > (((CAST((0.0000000001 / NULLIF(((CASE WHEN (CASE WHEN CAST((SELECT (SELECT 'alpha_beta_gamma' FROM tbl_07 AS inn_141 LIMIT 1) FROM tbl_04 AS sub_140 LIMIT 1) AS DECIMAL(10,2)) THEN NULLIF((SELECT CAST((ABS(sub_142.ref_tbl_01_id) - (sub_142.col_2 OR sub_142.id)) AS DECIMAL(10,2)) FROM tbl_04 AS sub_142 LIMIT 1), (('__fuzz_marker__' > (t_ref_39.col_2 + CAST((t_ref_39.ref_tbl_01_id IN (SELECT sub_143.col_2 FROM tbl_05 AS sub_143)) AS DECIMAL(10,2)))) <> (CASE WHEN (SELECT (SELECT inn_145.id FROM invoices AS inn_145 LIMIT 1) FROM invoices AS sub_144 LIMIT 1) THEN ((1.0 IN (SELECT sub_146.col_3 FROM tbl_06 AS sub_146)) + LENGTH(FALSE)) ELSE CAST(CAST(t_ref_39.col_4 AS SIGNED) AS CHAR) END))) ELSE 'ref_val' END) THEN t_ref_39.col_4 ELSE (CASE WHEN (t_ref_39.col_3 IN (SELECT sub_147.id FROM tbl_03 AS sub_147)) THEN (t_ref_39.ref_tbl_01_id + t_ref_39.id) ELSE (t_ref_39.ref_tbl_01_id * t_ref_39.col_4) END) END) < t_ref_39.col_4), 0)) AS SIGNED) / NULLIF('alpha_beta_gamma', 0)) / NULLIF(UPPER(((SELECT NULLIF(0, sub_150.ref_tbl_01_id) FROM tbl_02 AS sub_150 LIMIT 1) IS NULL)), 0)) OR (1e10 - (t_ref_39.ref_tbl_01_id + CAST(EXISTS (SELECT 1 FROM tbl_04 AS sub_151 WHERE TRUE) AS DECIMAL(10,2)))))) * ((-9223372036854775808 IS NULL) - (SELECT (SELECT NULL FROM tbl_03 AS inn_153 LIMIT 1) FROM tbl_07 AS sub_152 LIMIT 1))) END)
LIMIT 29;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON,
    col_3 NUMERIC(10,2),
    col_4 VARCHAR(64)
);

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 BOOLEAN,
    col_4 VARCHAR(64),
    ref_tbl_05_id INT,
    FOREIGN KEY (ref_tbl_05_id) REFERENCES tbl_05(id)
);

SELECT
    LENGTH((t_ref_154.id * (t_ref_154.col_1 IN (SELECT sub_155.payload FROM invoices AS sub_155)))) AS c_156,
    (CAST((t_ref_154.col_2 - CAST(3.14159265 AS CHAR)) AS SIGNED) AND CAST('ref_val' AS DECIMAL(10,2))) AS c_157
FROM tbl_05 AS t_ref_154 LEFT JOIN tbl_04 AS j_ref_158 ON t_ref_154.id = j_ref_158.id
WHERE (SELECT NULLIF(((((((sub_159.col_2 + CAST((sub_159.ref_tbl_01_id + (sub_159.col_2 * sub_159.col_1)) AS CHAR)) <> (sub_159.col_2 + NULLIF(LENGTH(CAST(3.14159265 AS SIGNED)), sub_159.ref_tbl_01_id))) / NULLIF((SELECT sub_160.id FROM invoices AS sub_160 LIMIT 1), 0)) > LENGTH(sub_159.col_2)) - (sub_159.ref_tbl_01_id / NULLIF(sub_159.id, 0))) / NULLIF((CASE WHEN ((CASE WHEN sub_159.col_2 THEN (((NOT (CAST((SELECT (SELECT '' FROM tbl_05 AS inn_162 LIMIT 1) FROM tbl_07 AS sub_161 LIMIT 1) AS SIGNED))) - COALESCE((SELECT ((CASE WHEN (((SELECT (SELECT inn_165.col_1 FROM tbl_07 AS inn_165 LIMIT 1) FROM invoices AS sub_164 LIMIT 1) BETWEEN 0 AND -9223372036854775808) - (SELECT sub_166.col_2 FROM tbl_04 AS sub_166 LIMIT 1)) THEN COALESCE(sub_163.col_1, ((CASE WHEN sub_163.col_1 THEN FALSE ELSE sub_163.id END) / NULLIF((SELECT sub_167.id FROM tbl_05 AS sub_167 LIMIT 1), 0))) ELSE (SELECT (-1.5 * sub_168.col_1) FROM tbl_03 AS sub_168 LIMIT 1) END) * sub_163.col_1) FROM tbl_07 AS sub_163 LIMIT 1), sub_159.col_2)) + EXISTS (SELECT 1 FROM invoices AS sub_169 WHERE sub_169.id)) ELSE (ABS(sub_159.id) <> NULL) END) < (NULL IN (SELECT sub_170.col_2 FROM tbl_06 AS sub_170))) THEN (sub_159.id IN (SELECT sub_175.col_1 FROM tbl_05 AS sub_175)) ELSE (sub_159.id IN (SELECT sub_185.col_1 FROM tbl_07 AS sub_185)) END), 0)), (EXISTS (SELECT 1 FROM tbl_03 AS sub_192 WHERE sub_192.col_1) - CAST((SELECT (NOT (LOWER(sub_194.col_2))) FROM tbl_09 AS sub_194 LIMIT 1) AS CHAR))) FROM tbl_02 AS sub_159 LIMIT 1)
LIMIT 8;

SELECT
    t_ref_195.id AS c_196
FROM tbl_07 AS t_ref_195
WHERE '__fuzz_marker__'
LIMIT 16;

CREATE INDEX idx_tbl_01_col_2_1 ON tbl_01 (col_2);

UPDATE tbl_05 SET col_1 = '__fuzz_marker__' WHERE id > 90;

UPDATE tbl_07 SET col_1 = '[]' WHERE id > 96;

DELETE FROM tbl_05 WHERE id = 95044;

CREATE INDEX idx_tbl_02_col_1_2 ON tbl_02 (col_1);

DELETE FROM tbl_07 WHERE id = 28648;

DELETE FROM tbl_02 WHERE id = 69950;

UPDATE tbl_03 SET col_1 = '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}' WHERE id > 48;

CREATE TABLE invoice_discounts (
    discount_id INT PRIMARY KEY,
    region_id INT NOT NULL,
    min_qualifying_amount DECIMAL(10,2) NOT NULL
);

SELECT
    CAST(CAST(CAST(t_ref_197.payload AS DECIMAL(10,2)) AS CHAR) AS DECIMAL(10,2)) AS c_198
FROM invoices AS t_ref_197 LEFT JOIN tbl_02 AS j_ref_199 ON t_ref_197.id = j_ref_199.id
WHERE (SELECT (SELECT inn_222.col_2 FROM tbl_05 AS inn_222 LIMIT 1) FROM tbl_08 AS sub_200 LIMIT 1)
LIMIT 22;

SELECT
    (SELECT (SELECT inn_225.col_1 FROM tbl_06 AS inn_225 LIMIT 1) FROM tbl_08 AS sub_224 LIMIT 1) AS c_226,
    (SELECT (SELECT 1.0 FROM tbl_03 AS inn_229 LIMIT 1) FROM tbl_04 AS sub_227 LIMIT 1) AS c_230,
    NULLIF((CASE WHEN (t_ref_223.col_4 - ((FALSE + (SELECT (SELECT inn_239.ref_tbl_02_id FROM tbl_03 AS inn_239 LIMIT 1) FROM tbl_05 AS sub_231 LIMIT 1)) - t_ref_223.col_2)) THEN (NOT ((CASE WHEN t_ref_223.col_1 THEN ((((((FALSE OR (CAST(FALSE AS DECIMAL(10,2)) / NULLIF(EXISTS (SELECT 1 FROM tbl_05 AS sub_240 WHERE TRUE), 0))) + (NOT (t_ref_223.col_4))) BETWEEN -9223372036854775808 AND 0) - COALESCE((CAST(42 AS DECIMAL(10,2)) + ('alpha_beta_gamma' + (((((t_ref_223.col_2 IN (SELECT sub_241.col_3 FROM tbl_08 AS sub_241)) AND CAST('ref_val' AS CHAR)) <> CAST((SELECT (SELECT 1 FROM tbl_07 AS inn_244 LIMIT 1) FROM tbl_08 AS sub_242 LIMIT 1) AS SIGNED)) IS NULL) * NULLIF(t_ref_223.col_2, (NOT (t_ref_223.col_4)))))), t_ref_223.col_2)) = (SELECT (((SELECT (CAST(LOWER(CAST(CAST(NULL AS SIGNED) AS CHAR)) AS SIGNED) / NULLIF(sub_246.col_1, 0)) FROM tbl_04 AS sub_246 LIMIT 1) * (9223372036854775807 / NULLIF((EXISTS (SELECT 1 FROM tbl_05 AS sub_247 WHERE 3.14159265) + sub_245.payload), 0))) + (CASE WHEN (SELECT CAST(((COALESCE(sub_250.col_2, sub_250.col_1) / NULLIF(CAST(EXISTS (SELECT 1 FROM tbl_05 AS sub_251 WHERE sub_251.col_1) AS DECIMAL(10,2)), 0)) + ABS((SELECT (SELECT -1.5 FROM tbl_04 AS inn_253 LIMIT 1) FROM tbl_05 AS sub_252 LIMIT 1))) AS CHAR) FROM tbl_06 AS sub_250 LIMIT 1) THEN sub_245.payload ELSE sub_245.payload END)) FROM invoices AS sub_245 LIMIT 1)) * LOWER(((((LOWER(COALESCE((t_ref_223.id / NULLIF(CAST(LOWER(t_ref_223.id) AS CHAR), 0)), (t_ref_223.col_1 IN (SELECT sub_254.col_1 FROM tbl_01 AS sub_254)))) / NULLIF((t_ref_223.col_3 IN (SELECT sub_255.col_4 FROM tbl_04 AS sub_255)), 0)) / NULLIF(LOWER(t_ref_223.col_3), 0)) AND CAST((LENGTH(((COALESCE((SELECT CAST(sub_256.id AS CHAR) FROM invoices AS sub_256 LIMIT 1), t_ref_223.id) + (CAST(EXISTS (SELECT 1 FROM tbl_05 AS sub_257 WHERE FALSE) AS CHAR) / NULLIF(NULLIF(CAST((CASE WHEN t_ref_223.col_4 THEN t_ref_223.col_2 ELSE '__fuzz_marker__' END) AS CHAR), ((t_ref_223.col_4 - 2147483647) / NULLIF(t_ref_223.id, 0))), 0))) + t_ref_223.col_3)) - (SELECT (sub_258.col_2 IN (SELECT sub_259.id FROM tbl_05 AS sub_259)) FROM tbl_05 AS sub_258 LIMIT 1)) AS DECIMAL(10,2))) / NULLIF(((NULL IN (SELECT sub_261.id FROM invoices AS sub_261)) * LOWER(t_ref_223.col_2)), 0)))) ELSE ((CASE WHEN (SELECT (SELECT inn_263.col_1 FROM tbl_07 AS inn_263 LIMIT 1) FROM tbl_07 AS sub_262 LIMIT 1) THEN (t_ref_223.col_2 / NULLIF(CAST((SELECT (SELECT inn_265.col_2 FROM tbl_05 AS inn_265 LIMIT 1) FROM tbl_02 AS sub_264 LIMIT 1) AS SIGNED), 0)) ELSE (NULL IN (SELECT sub_266.id FROM tbl_02 AS sub_266)) END) <= EXISTS (SELECT 1 FROM tbl_09 AS sub_267 WHERE sub_267.col_4)) END))) ELSE LENGTH(LENGTH(((1 IN (SELECT sub_268.col_2 FROM tbl_08 AS sub_268)) * LOWER(((SELECT (SELECT inn_274.id FROM tbl_07 AS inn_274 LIMIT 1) FROM tbl_09 AS sub_273 LIMIT 1) <> (((SELECT (SELECT ((((sub_276.col_2 >= sub_276.col_3) / NULLIF(UPPER(sub_276.col_1), 0)) / NULLIF((sub_276.col_4 - CAST(sub_276.col_4 AS CHAR)), 0)) / NULLIF(sub_276.col_2, 0)) FROM tbl_04 AS sub_276 LIMIT 1) FROM tbl_04 AS sub_275 LIMIT 1) <= (1e10 IS NOT NULL)) BETWEEN -2147483648 AND 2147483647)))))) END), CAST((EXISTS (SELECT 1 FROM tbl_07 AS sub_277 WHERE sub_277.col_2) AND t_ref_223.col_2) AS CHAR)) AS c_279
FROM tbl_08 AS t_ref_223
WHERE (t_ref_223.col_4 IS NOT NULL)
LIMIT 4;

UPDATE invoices SET payload = 'alpha_beta_gamma' WHERE id > 9;

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 JSON,
    col_3 INT
);

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

UPDATE tbl_02 SET col_2 = FALSE WHERE id > 68;

INSERT INTO tbl_09 (id, col_1, col_2, col_3, col_4, ref_tbl_05_id) VALUES (794648, '', TRUE, FALSE, '__fuzz_marker__', -9223372036854775808);

SELECT
    -9223372036854775808 AS c_281,
    UPPER((((SELECT sub_282.id FROM invoices AS sub_282 LIMIT 1) BETWEEN 1 AND -1) / NULLIF(t_ref_280.id, 0))) AS c_283,
    t_ref_280.col_2 AS c_284
FROM tbl_09 AS t_ref_280
WHERE (CAST(t_ref_280.id AS CHAR) = t_ref_280.col_1)
LIMIT 27;

CREATE INDEX idx_tbl_02_col_2_3 ON tbl_02 (col_2);

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT,
    col_3 INT
);

CREATE INDEX idx_tbl_04_col_2_4 ON tbl_04 (col_2);

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2)
);

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_01 WHERE id IS NOT NULL;

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 JSON
);

DELETE FROM tbl_03 WHERE id = 80797;

UPDATE tbl_07 SET col_2 = 'ref_val' WHERE id > 61;

DELETE FROM tbl_06 WHERE id = 74330;

SELECT
    CAST(t_ref_285.col_2 AS DECIMAL(10,2)) AS c_286,
    ((SELECT sub_287.ref_tbl_01_id FROM tbl_02 AS sub_287 LIMIT 1) + (((COALESCE((CASE WHEN (SELECT sub_288.id FROM tbl_14 AS sub_288 LIMIT 1) THEN EXISTS (SELECT 1 FROM tbl_05 AS sub_289 WHERE 'chaos') ELSE CAST(1.0 AS CHAR) END), ('' IN (SELECT sub_290.col_2 FROM tbl_14 AS sub_290))) <> ((SELECT (SELECT inn_318.col_1 FROM tbl_05 AS inn_318 LIMIT 1) FROM tbl_03 AS sub_302 LIMIT 1) IS NULL)) + t_ref_285.id) - ((SELECT (SELECT inn_328.col_2 FROM tbl_05 AS inn_328 LIMIT 1) FROM tbl_12 AS sub_319 LIMIT 1) / NULLIF((t_ref_285.id IN (SELECT sub_329.id FROM tbl_02 AS sub_329)), 0)))) AS c_333
FROM tbl_06 AS t_ref_285 LEFT JOIN tbl_13 AS j_ref_334 ON t_ref_285.id = j_ref_334.id
WHERE CAST(t_ref_285.col_1 AS SIGNED)
LIMIT 2;

INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_02_id) VALUES (786422, '[]', TRUE, -9223372036854775808);

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2)
);

INSERT INTO tbl_02 (id, col_1, col_2, ref_tbl_01_id) VALUES (327774, 0.0000000001, FALSE, -1);

INSERT INTO invoices (invoice_id, region_id, sales_rep_id, invoice_amount) VALUES
    (1, 10, 101, 5000.00),
    (2, 10, 101, 7500.00),
    (3, 10, 102, 3200.00),
    (4, 20, 201, 12000.00),
    (5, 20, 202, 4500.00);

INSERT INTO invoices (id, payload) VALUES (490445, '__fuzz_marker__');

DELETE FROM tbl_11 WHERE id = 80466;

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2)
);

CREATE TABLE tbl_17 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 JSON
);

CREATE TABLE tbl_18 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 NUMERIC(10,2),
    col_4 INT,
    ref_tbl_11_id INT,
    FOREIGN KEY (ref_tbl_11_id) REFERENCES tbl_11(id)
);

INSERT INTO tbl_10 (id, col_1, col_2, col_3) VALUES (722310, TRUE, '[]', 1);

CREATE INDEX idx_tbl_15_col_1_5 ON tbl_15 (col_1);

CREATE TABLE tbl_19 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSON,
    col_3 JSON,
    col_4 BOOLEAN
);

CREATE TABLE tbl_20 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2),
    col_3 VARCHAR(64),
    col_4 JSON,
    ref_tbl_11_id INT,
    FOREIGN KEY (ref_tbl_11_id) REFERENCES tbl_11(id)
);

CREATE INDEX idx_tbl_09_col_3_6 ON tbl_09 (col_3);

INSERT INTO tbl_18 (id, col_1, col_2, col_3, col_4, ref_tbl_11_id) VALUES (793310, 'escaped''quote', 'alpha_beta_gamma', 3.14159265, 42, -2147483648);

SELECT
    (NOT ((SELECT (SELECT 1e10 FROM tbl_20 AS inn_338 LIMIT 1) FROM tbl_06 AS sub_336 LIMIT 1))) AS c_339,
    FALSE AS c_340
FROM tbl_17 AS t_ref_335
WHERE (LENGTH((EXISTS (SELECT 1 FROM tbl_01 AS sub_341 WHERE sub_341.id) / NULLIF(t_ref_335.col_1, 0))) * EXISTS (SELECT 1 FROM tbl_16 AS sub_361 WHERE sub_361.id))
LIMIT 13;

CREATE INDEX idx_tbl_08_id_7 ON tbl_08 (id);

CREATE TABLE tbl_21 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 BOOLEAN,
    ref_tbl_09_id INT,
    FOREIGN KEY (ref_tbl_09_id) REFERENCES tbl_09(id)
);

INSERT INTO tbl_13 (id, col_1, col_2) VALUES (694233, TRUE, 3.14159265);

UPDATE invoices SET payload = 'escaped''quote' WHERE id > 22;

BEGIN;
INSERT INTO tbl_12 (id, col_1, col_2, col_3) VALUES (487513, FALSE, 100, -2147483648);
COMMIT;

CREATE TABLE tbl_22 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2)
);

INSERT INTO tbl_18 (id, col_1, col_2, col_3, col_4, ref_tbl_11_id) VALUES (790967, 'chaos', '__fuzz_marker__', 3.14159265, -1, -2147483648);

BEGIN;
INSERT INTO tbl_21 (id, col_1, col_2, ref_tbl_09_id) VALUES (122175, '[]', FALSE, 9223372036854775807);
INSERT INTO invoices (id, payload) VALUES (626181, 'chaos');
INSERT INTO tbl_19 (id, col_1, col_2, col_3, col_4) VALUES (109571, 'chaos', '{}', '[]', FALSE);
COMMIT;

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

INSERT INTO invoice_discounts (discount_id, region_id, min_qualifying_amount) VALUES
    (1, 10, 4000.00),
    (2, 20, 10000.00);

BEGIN;
INSERT INTO invoices (id, payload) VALUES (661390, '__fuzz_marker__');
COMMIT;

SELECT
    t_ref_378.id AS c_379,
    (t_ref_378.id IN (SELECT sub_380.col_1 FROM tbl_14 AS sub_380)) AS c_381,
    TRUE AS c_382
FROM tbl_16 AS t_ref_378 LEFT JOIN tbl_01 AS j_ref_383 ON t_ref_378.id = j_ref_383.id
WHERE (t_ref_378.col_1 IN (SELECT sub_384.col_1 FROM tbl_16 AS sub_384))
LIMIT 44;

INSERT INTO tbl_13 (id, col_1, col_2) VALUES (510959, FALSE, 0.0);

INSERT INTO tbl_19 (id, col_1, col_2, col_3, col_4) VALUES (48147, '__fuzz_marker__', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '[]', TRUE);

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2) VALUES (684789, '', TRUE);
INSERT INTO tbl_09 (id, col_1, col_2, col_3, col_4, ref_tbl_05_id) VALUES (633467, '', FALSE, TRUE, 'ref_val', 42);
INSERT INTO tbl_15 (id, col_1, col_2, col_3) VALUES (843827, TRUE, -1.5, 0.0000000001);
COMMIT;

CREATE TABLE tbl_23 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 BOOLEAN,
    col_4 JSON,
    ref_tbl_14_id INT,
    FOREIGN KEY (ref_tbl_14_id) REFERENCES tbl_14(id)
);

INSERT INTO tbl_01 (id, col_1, col_2) VALUES (663223, '__fuzz_marker__', '{}');

UPDATE tbl_11 SET col_2 = '' WHERE id > 95;

SELECT
    t_ref_386.col_2 AS c_387,
    (t_ref_386.col_2 + EXISTS (SELECT 1 FROM tbl_20 AS sub_388 WHERE sub_388.ref_tbl_11_id)) AS c_397
FROM tbl_21 AS t_ref_386 LEFT JOIN tbl_11 AS j_ref_398 ON t_ref_386.id = j_ref_398.id
WHERE t_ref_386.col_2
LIMIT 14;

INSERT INTO tbl_13 (id, col_1, col_2) VALUES (371017, TRUE, 999999999.99);

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (853605, '{}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 0.0, '__fuzz_marker__');

BEGIN;
INSERT INTO tbl_15 (id, col_1, col_2, col_3) VALUES (95851, TRUE, -1.5, -1e-5);
INSERT INTO tbl_14 (id, col_1, col_2) VALUES (552978, FALSE, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
INSERT INTO tbl_12 (id, col_1, col_2, col_3) VALUES (46284, FALSE, 100, 1);
COMMIT;

CREATE TABLE tbl_24 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 BOOLEAN,
    col_4 VARCHAR(64)
);

CREATE TABLE tbl_25 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT
);

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (544689, '{}', '{}', 3.14159265, 'escaped''quote');

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_19 WHERE id IS NOT NULL;

CREATE TABLE tbl_26 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 BOOLEAN,
    col_3 NUMERIC(10,2),
    ref_tbl_12_id INT,
    FOREIGN KEY (ref_tbl_12_id) REFERENCES tbl_12(id)
);

CREATE TABLE tbl_27 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    ref_tbl_14_id INT,
    FOREIGN KEY (ref_tbl_14_id) REFERENCES tbl_14(id)
);

BEGIN;
INSERT INTO invoices (id, payload) VALUES (87506, '');
INSERT INTO tbl_15 (id, col_1, col_2, col_3) VALUES (688439, TRUE, 0.0000000001, 1.0);
INSERT INTO tbl_07 (id, col_1, col_2) VALUES (448792, '[]', '__fuzz_marker__');
COMMIT;

CREATE TABLE tbl_28 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 INT,
    col_3 VARCHAR(64),
    ref_tbl_27_id INT,
    FOREIGN KEY (ref_tbl_27_id) REFERENCES tbl_27(id)
);

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_26 WHERE id IS NOT NULL;

INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (525852, FALSE, 0.0, '[]');

CREATE TABLE tbl_29 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    col_3 NUMERIC(10,2)
);

SELECT
    LENGTH(t_ref_399.col_1) AS c_400,
    (t_ref_399.col_2 * ((NULLIF((SELECT (SELECT inn_403.col_2 FROM tbl_08 AS inn_403 LIMIT 1) FROM tbl_19 AS sub_401 LIMIT 1), (((SELECT (sub_404.col_3 IN (SELECT sub_405.col_1 FROM tbl_14 AS sub_405)) FROM tbl_12 AS sub_404 LIMIT 1) + t_ref_399.id) IS NULL)) * t_ref_399.id) IS NOT NULL)) AS c_406
FROM tbl_17 AS t_ref_399 LEFT JOIN tbl_05 AS j_ref_407 ON t_ref_399.id = j_ref_407.id
WHERE ((COALESCE(NULL, ((t_ref_399.col_2 BETWEEN 2147483647 AND 2147483647) - (CASE WHEN (9223372036854775807 IN (SELECT sub_408.col_2 FROM tbl_05 AS sub_408)) THEN t_ref_399.col_1 ELSE UPPER((t_ref_399.id / NULLIF(CAST(LENGTH(CAST(CAST(LOWER(t_ref_399.id) AS SIGNED) AS CHAR)) AS CHAR), 0))) END))) / NULLIF(t_ref_399.col_1, 0)) AND t_ref_399.id)
LIMIT 46;

INSERT INTO tbl_26 (id, col_1, col_2, col_3, ref_tbl_12_id) VALUES (199952, 999999999.99, TRUE, 3.14159265, 100);

CREATE TABLE tbl_30 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 NUMERIC(10,2),
    col_3 BOOLEAN,
    ref_tbl_23_id INT,
    FOREIGN KEY (ref_tbl_23_id) REFERENCES tbl_23(id)
);

INSERT INTO tbl_01 (id, col_1, col_2) VALUES (2574, 'escaped''quote', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

CREATE INDEX idx_tbl_23_col_4_8 ON tbl_23 (col_4);

INSERT INTO tbl_07 (id, col_1, col_2) VALUES (123591, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '');

SELECT
    -1 AS c_414,
    (SELECT NULL FROM tbl_13 AS sub_415 LIMIT 1) AS c_416
FROM tbl_05 AS t_ref_413
WHERE ((SELECT CAST((sub_417.col_3 / NULLIF(((CASE WHEN (EXISTS (SELECT 1 FROM tbl_27 AS sub_418 WHERE sub_418.id) * (CASE WHEN NULLIF(2147483647, COALESCE((sub_417.col_3 IN (SELECT sub_426.ref_tbl_01_id FROM tbl_02 AS sub_426)), (CAST((LENGTH(sub_417.ref_tbl_27_id) OR sub_417.col_3) AS SIGNED) - 1))) THEN CAST(CAST(((sub_417.col_1 IN (SELECT sub_427.col_2 FROM tbl_01 AS sub_427)) AND (sub_417.col_2 IN (SELECT sub_428.col_3 FROM tbl_24 AS sub_428))) AS DECIMAL(10,2)) AS CHAR) ELSE ((sub_417.col_1 / NULLIF(sub_417.col_3, 0)) - sub_417.col_2) END)) THEN 'ref_val' ELSE ((sub_417.ref_tbl_27_id BETWEEN 0 AND -2147483648) - (CAST(((sub_417.col_1 AND (SELECT (((TRUE + sub_430.col_2) * (-1 IN (SELECT sub_431.col_2 FROM tbl_22 AS sub_431))) - COALESCE((sub_430.col_4 >= sub_430.id), (sub_430.col_3 IN (SELECT sub_432.col_2 FROM tbl_17 AS sub_432)))) FROM tbl_24 AS sub_430 LIMIT 1)) + (CASE WHEN (SELECT (SELECT inn_434.col_1 FROM tbl_14 AS inn_434 LIMIT 1) FROM tbl_15 AS sub_433 LIMIT 1) THEN ABS((-1e-5 >= (UPPER(sub_417.col_2) - (SELECT sub_435.col_2 FROM tbl_14 AS sub_435 LIMIT 1)))) ELSE sub_417.col_1 END)) AS SIGNED) >= ABS(NULLIF((NOT (-9223372036854775808)), (SELECT (SELECT inn_437.col_3 FROM tbl_08 AS inn_437 LIMIT 1) FROM tbl_10 AS sub_436 LIMIT 1))))) END) - (TRUE IS NULL)), 0)) AS CHAR) FROM tbl_28 AS sub_417 LIMIT 1) BETWEEN 1 AND 2147483647)
LIMIT 50;

INSERT INTO tbl_18 (id, col_1, col_2, col_3, col_4, ref_tbl_11_id) VALUES (316767, 'chaos', 'escaped''quote', -1.5, -9223372036854775808, -1);

UPDATE tbl_21 SET col_2 = FALSE WHERE id > 25;

SELECT
    (((CASE WHEN (CASE WHEN t_ref_438.ref_tbl_05_id THEN (t_ref_438.col_1 + ((CAST(LOWER((t_ref_438.col_4 <= t_ref_438.col_1)) AS DECIMAL(10,2)) + CAST(((LOWER((TRUE IN (SELECT sub_439.id FROM tbl_21 AS sub_439))) / NULLIF((ABS(LOWER(EXISTS (SELECT 1 FROM tbl_22 AS sub_440 WHERE sub_440.col_2))) OR 0.0000000001), 0)) + (SELECT EXISTS (SELECT 1 FROM tbl_05 AS sub_443 WHERE sub_443.col_1) FROM tbl_22 AS sub_442 LIMIT 1)) AS CHAR)) AND ((SELECT UPPER(ABS(NULL)) FROM tbl_23 AS sub_444 LIMIT 1) * (COALESCE((((CASE WHEN t_ref_438.col_1 THEN (SELECT (SELECT inn_450.col_3 FROM tbl_20 AS inn_450 LIMIT 1) FROM tbl_08 AS sub_445 LIMIT 1) ELSE CAST((CASE WHEN (t_ref_438.col_3 <> UPPER((SELECT sub_451.id FROM tbl_13 AS sub_451 LIMIT 1))) THEN 0 ELSE (((NULL >= 100) > (NULL - t_ref_438.col_3)) * (CASE WHEN CAST(t_ref_438.id AS DECIMAL(10,2)) THEN 0.0 ELSE (t_ref_438.col_4 < 42) END)) END) AS CHAR) END) * 3.14159265) / NULLIF(((t_ref_438.ref_tbl_05_id - ((SELECT (NULL + (2147483647 IN (SELECT sub_453.id FROM tbl_16 AS sub_453))) FROM tbl_03 AS sub_452 LIMIT 1) - EXISTS (SELECT 1 FROM tbl_15 AS sub_454 WHERE NULL))) <= (LOWER((SELECT 100 FROM tbl_16 AS sub_457 LIMIT 1)) * t_ref_438.ref_tbl_05_id)), 0)), (CAST((SELECT (SELECT 100 FROM tbl_08 AS inn_459 LIMIT 1) FROM tbl_29 AS sub_458 LIMIT 1) AS SIGNED) < CAST(LENGTH((SELECT (SELECT NULL FROM tbl_21 AS inn_462 LIMIT 1) FROM tbl_10 AS sub_460 LIMIT 1)) AS CHAR))) - (UPPER(EXISTS (SELECT 1 FROM tbl_07 AS sub_463 WHERE 0.0)) AND CAST((FALSE * t_ref_438.col_4) AS CHAR)))))) ELSE '' END) THEN EXISTS (SELECT 1 FROM tbl_12 AS sub_468 WHERE 0.0000000001) ELSE (((-1.5 IN (SELECT sub_508.payload FROM invoices AS sub_508)) <> CAST((-2147483648 IN (SELECT sub_509.col_1 FROM tbl_25 AS sub_509)) AS DECIMAL(10,2))) / NULLIF(((NOT ((t_ref_438.col_2 IN (SELECT sub_510.ref_tbl_14_id FROM tbl_23 AS sub_510)))) / NULLIF((SELECT (SELECT NULL FROM tbl_09 AS inn_520 LIMIT 1) FROM tbl_20 AS sub_517 LIMIT 1), 0)), 0)) END) < -9223372036854775808) > t_ref_438.col_4) AS c_521
FROM tbl_09 AS t_ref_438
WHERE (SELECT (SELECT 'ref_val' FROM tbl_03 AS inn_523 LIMIT 1) FROM tbl_19 AS sub_522 LIMIT 1)
LIMIT 49;

BEGIN;
INSERT INTO tbl_17 (id, col_1, col_2) VALUES (591128, -2147483648, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
INSERT INTO tbl_14 (id, col_1, col_2) VALUES (883538, FALSE, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_02_id) VALUES (501513, '[]', FALSE, 0);
COMMIT;

CREATE TABLE tbl_31 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2),
    col_4 NUMERIC(10,2),
    ref_tbl_27_id INT,
    FOREIGN KEY (ref_tbl_27_id) REFERENCES tbl_27(id)
);

UPDATE tbl_31 SET col_3 = 0.0000000001 WHERE id > 97;

INSERT INTO tbl_31 (id, col_1, col_2, col_3, col_4, ref_tbl_27_id) VALUES (495544, 'alpha_beta_gamma', -1.5, 3.14159265, 1.0, -1);

ALTER TABLE tbl_07 ADD COLUMN dyn_col_4 VARCHAR(32);

INSERT INTO tbl_18 (id, col_1, col_2, col_3, col_4, ref_tbl_11_id) VALUES (540439, 'alpha_beta_gamma', 'alpha_beta_gamma', 0.0, -1, 0);

SELECT
    ((t_ref_524.ref_tbl_27_id / NULLIF((SELECT EXISTS (SELECT 1 FROM tbl_10 AS sub_526 WHERE sub_526.id) FROM tbl_19 AS sub_525 LIMIT 1), 0)) - (ABS(UPPER(((SELECT (SELECT -1.5 FROM invoices AS inn_533 LIMIT 1) FROM tbl_11 AS sub_532 LIMIT 1) AND (t_ref_524.col_3 IS NULL)))) - t_ref_524.ref_tbl_27_id)) AS c_534,
    ((NULLIF((((SELECT (SELECT inn_540.col_4 FROM tbl_31 AS inn_540 LIMIT 1) FROM tbl_23 AS sub_535 LIMIT 1) - ABS(1.0)) OR ((CASE WHEN (t_ref_524.col_2 * (SELECT (LOWER((0 IN (SELECT sub_542.id FROM tbl_26 AS sub_542))) - sub_541.col_2) FROM tbl_22 AS sub_541 LIMIT 1)) THEN (CASE WHEN t_ref_524.col_2 THEN t_ref_524.id ELSE TRUE END) ELSE 1 END) * COALESCE(CAST(CAST((t_ref_524.col_1 IN (SELECT sub_543.id FROM tbl_28 AS sub_543)) AS DECIMAL(10,2)) AS SIGNED), t_ref_524.col_1))), LENGTH(NULLIF((EXISTS (SELECT 1 FROM tbl_04 AS sub_548 WHERE TRUE) * (SELECT (SELECT inn_558.col_3 FROM tbl_12 AS inn_558 LIMIT 1) FROM tbl_18 AS sub_557 LIMIT 1)), NULLIF(t_ref_524.id, (SELECT (sub_559.col_1 * sub_559.id) FROM tbl_15 AS sub_559 LIMIT 1))))) - (EXISTS (SELECT 1 FROM tbl_28 AS sub_560 WHERE 'escaped''quote') <> (((NULLIF((UPPER((t_ref_524.id IN (SELECT sub_566.id FROM tbl_24 AS sub_566))) OR 'ref_val'), NULLIF(((((t_ref_524.col_1 IN (SELECT sub_570.col_3 FROM tbl_19 AS sub_570)) + ((t_ref_524.id IN (SELECT sub_571.id FROM tbl_16 AS sub_571)) > ((((SELECT sub_573.id FROM tbl_30 AS sub_573 LIMIT 1) + NULLIF(('__fuzz_marker__' < t_ref_524.ref_tbl_27_id), ('alpha_beta_gamma' < t_ref_524.col_3))) - FALSE) / NULLIF(t_ref_524.id, 0)))) + (SELECT (SELECT 1.0 FROM tbl_24 AS inn_576 LIMIT 1) FROM tbl_12 AS sub_574 LIMIT 1)) * COALESCE((UPPER(((SELECT (SELECT TRUE FROM tbl_27 AS inn_579 LIMIT 1) FROM tbl_14 AS sub_577 LIMIT 1) = (SELECT CAST((sub_580.ref_tbl_09_id / NULLIF(sub_580.id, 0)) AS CHAR) FROM tbl_21 AS sub_580 LIMIT 1))) > LOWER((EXISTS (SELECT 1 FROM tbl_16 AS sub_581 WHERE sub_581.id) - (ABS(((SELECT sub_582.col_2 FROM tbl_25 AS sub_582 LIMIT 1) / NULLIF((t_ref_524.id IN (SELECT sub_583.col_2 FROM tbl_05 AS sub_583)), 0))) AND t_ref_524.col_1)))), (SELECT sub_584.col_2 FROM tbl_17 AS sub_584 LIMIT 1))), LENGTH(((t_ref_524.col_1 + LOWER(((UPPER(-2147483648) / NULLIF((CAST((TRUE - t_ref_524.id) AS CHAR) >= ((SELECT (SELECT inn_586.col_1 FROM tbl_22 AS inn_586 LIMIT 1) FROM tbl_30 AS sub_585 LIMIT 1) - (NOT (t_ref_524.id)))), 0)) * CAST(NULLIF(UPPER((SELECT (SELECT inn_588.col_4 FROM tbl_20 AS inn_588 LIMIT 1) FROM tbl_24 AS sub_587 LIMIT 1)), (t_ref_524.col_3 IN (SELECT sub_589.ref_tbl_11_id FROM tbl_20 AS sub_589))) AS DECIMAL(10,2))))) >= (EXISTS (SELECT 1 FROM tbl_14 AS sub_590 WHERE sub_590.id) * EXISTS (SELECT 1 FROM tbl_26 AS sub_591 WHERE sub_591.col_3)))))) <= (SELECT (SELECT inn_599.col_2 FROM tbl_17 AS inn_599 LIMIT 1) FROM tbl_16 AS sub_595 LIMIT 1)) IS NULL) <> CAST((((t_ref_524.id <> (CAST((SELECT (SELECT 9223372036854775807 FROM tbl_06 AS inn_603 LIMIT 1) FROM tbl_11 AS sub_600 LIMIT 1) AS SIGNED) + UPPER(t_ref_524.col_3))) - NULLIF((CAST(t_ref_524.col_2 AS DECIMAL(10,2)) IS NULL), (SELECT (SELECT 0.0000000001 FROM tbl_20 AS inn_605 LIMIT 1) FROM tbl_29 AS sub_604 LIMIT 1))) <= (t_ref_524.col_2 / NULLIF(t_ref_524.ref_tbl_27_id, 0))) AS DECIMAL(10,2))))) * ((((((CAST(t_ref_524.col_2 AS DECIMAL(10,2)) < (CAST(((ABS((EXISTS (SELECT 1 FROM tbl_18 AS sub_606 WHERE sub_606.ref_tbl_11_id) + (COALESCE(((CASE WHEN '' THEN t_ref_524.col_1 ELSE -1.5 END) - (t_ref_524.col_3 * FALSE)), (CAST(t_ref_524.col_3 AS DECIMAL(10,2)) BETWEEN 0 AND 2147483647)) <> LENGTH(t_ref_524.ref_tbl_27_id)))) IS NOT NULL) + EXISTS (SELECT 1 FROM tbl_10 AS sub_608 WHERE sub_608.col_1)) AS CHAR) > LOWER((CASE WHEN (SELECT (SELECT inn_612.col_1 FROM tbl_02 AS inn_612 LIMIT 1) FROM tbl_06 AS sub_611 LIMIT 1) THEN ((SELECT (SELECT -1 FROM tbl_25 AS inn_614 LIMIT 1) FROM tbl_10 AS sub_613 LIMIT 1) / NULLIF((CASE WHEN t_ref_524.col_2 THEN CAST(ABS(LOWER(((t_ref_524.col_1 + t_ref_524.col_3) > (t_ref_524.col_3 IN (SELECT sub_615.col_2 FROM tbl_08 AS sub_615))))) AS CHAR) ELSE (SELECT (CAST((CASE WHEN sub_616.col_1 THEN sub_616.id ELSE sub_616.col_2 END) AS DECIMAL(10,2)) * ((TRUE OR sub_616.id) + (sub_616.col_2 / NULLIF(-1.5, 0)))) FROM tbl_05 AS sub_616 LIMIT 1) END), 0)) ELSE ((((SELECT (SELECT (SELECT inn_619.col_1 FROM tbl_16 AS inn_619 LIMIT 1) FROM tbl_01 AS sub_618 LIMIT 1) FROM tbl_14 AS sub_617 LIMIT 1) / NULLIF(CAST((SELECT (NOT (sub_620.col_2)) FROM tbl_11 AS sub_620 LIMIT 1) AS CHAR), 0)) <> EXISTS (SELECT 1 FROM tbl_24 AS sub_621 WHERE sub_621.col_4)) BETWEEN 9223372036854775807 AND 9223372036854775807) END)))) / NULLIF((SELECT NULLIF(sub_624.col_1, ((CAST(sub_624.col_2 AS SIGNED) IS NOT NULL) OR (NOT (((sub_624.id - ((TRUE IN (SELECT sub_625.col_1 FROM tbl_29 AS sub_625)) > sub_624.col_2)) BETWEEN 0 AND 100))))) FROM tbl_05 AS sub_624 LIMIT 1), 0)) AND (CAST((CAST((t_ref_524.col_3 IN (SELECT sub_626.col_2 FROM tbl_01 AS sub_626)) AS CHAR) < ((FALSE IN (SELECT sub_627.ref_tbl_14_id FROM tbl_27 AS sub_627)) - UPPER(NULLIF((((CAST((SELECT sub_628.id FROM tbl_26 AS sub_628 LIMIT 1) AS SIGNED) >= ((('alpha_beta_gamma' < t_ref_524.col_2) + (t_ref_524.col_2 AND t_ref_524.col_3)) * (9223372036854775807 / NULLIF(UPPER(NULL), 0)))) BETWEEN 9223372036854775807 AND 0) > t_ref_524.id), CAST(t_ref_524.col_3 AS SIGNED))))) AS CHAR) IS NULL)) * (SELECT (sub_629.col_1 + (sub_629.col_1 AND (CASE WHEN ((SELECT FALSE FROM tbl_25 AS sub_630 LIMIT 1) - ('alpha_beta_gamma' < (CASE WHEN (((CASE WHEN ((TRUE - -1e-5) OR (sub_629.ref_tbl_04_id OR sub_629.ref_tbl_04_id)) THEN ((-2147483648 >= TRUE) * EXISTS (SELECT 1 FROM tbl_24 AS sub_631 WHERE 3.14159265)) ELSE (sub_629.col_2 OR UPPER(sub_629.col_2)) END) / NULLIF(EXISTS (SELECT 1 FROM tbl_10 AS sub_632 WHERE sub_632.id), 0)) IS NULL) THEN (SELECT (EXISTS (SELECT 1 FROM tbl_24 AS sub_634 WHERE sub_634.id) < EXISTS (SELECT 1 FROM tbl_17 AS sub_635 WHERE sub_635.col_2)) FROM tbl_17 AS sub_633 LIMIT 1) ELSE ABS((((sub_629.col_1 <> CAST(sub_629.id AS CHAR)) + EXISTS (SELECT 1 FROM tbl_25 AS sub_636 WHERE sub_636.col_1)) * (FALSE / NULLIF(((NOT (TRUE)) AND UPPER(sub_629.col_1)), 0)))) END))) THEN CAST(COALESCE((COALESCE(((sub_629.ref_tbl_04_id IN (SELECT sub_637.col_2 FROM tbl_05 AS sub_637)) < ((sub_629.col_2 BETWEEN 2147483647 AND 1) BETWEEN 2147483647 AND -2147483648)), ((sub_629.ref_tbl_04_id OR CAST((sub_629.ref_tbl_04_id AND TRUE) AS DECIMAL(10,2))) / NULLIF(((CASE WHEN EXISTS (SELECT 1 FROM tbl_28 AS sub_638 WHERE TRUE) THEN (SELECT (SELECT inn_640.col_3 FROM tbl_30 AS inn_640 LIMIT 1) FROM tbl_01 AS sub_639 LIMIT 1) ELSE (SELECT (SELECT inn_642.col_1 FROM tbl_10 AS inn_642 LIMIT 1) FROM tbl_17 AS sub_641 LIMIT 1) END) < CAST((FALSE <= sub_629.col_2) AS CHAR)), 0))) - EXISTS (SELECT 1 FROM tbl_08 AS sub_643 WHERE sub_643.id)), ((SELECT ((CAST(sub_645.id AS SIGNED) / NULLIF((SELECT sub_646.col_3 FROM tbl_29 AS sub_646 LIMIT 1), 0)) * FALSE) FROM tbl_04 AS sub_645 LIMIT 1) >= (CASE WHEN NULLIF((sub_629.id IN (SELECT sub_647.col_2 FROM tbl_13 AS sub_647)), CAST(sub_629.id AS CHAR)) THEN (SELECT (SELECT inn_649.id FROM tbl_27 AS inn_649 LIMIT 1) FROM tbl_04 AS sub_648 LIMIT 1) ELSE ((CASE WHEN EXISTS (SELECT 1 FROM tbl_14 AS sub_650 WHERE sub_650.id) THEN (NULLIF(sub_629.id, sub_629.ref_tbl_04_id) < (sub_629.col_2 * NULL)) ELSE EXISTS (SELECT 1 FROM tbl_17 AS sub_651 WHERE sub_651.col_1) END) - (((sub_629.col_2 * sub_629.col_1) / NULLIF((sub_629.col_1 AND 999999999.99), 0)) BETWEEN 100 AND 42)) END))) AS SIGNED) ELSE CAST(((CASE WHEN COALESCE(sub_629.col_1, NULLIF((SELECT (SELECT (SELECT inn_654.col_1 FROM tbl_22 AS inn_654 LIMIT 1) FROM tbl_08 AS sub_653 LIMIT 1) FROM tbl_31 AS sub_652 LIMIT 1), (SELECT (SELECT inn_656.col_4 FROM tbl_04 AS inn_656 LIMIT 1) FROM tbl_10 AS sub_655 LIMIT 1))) THEN sub_629.ref_tbl_04_id ELSE EXISTS (SELECT 1 FROM tbl_20 AS sub_657 WHERE sub_657.col_1) END) - (CAST((sub_629.col_1 IN (SELECT sub_658.col_1 FROM tbl_31 AS sub_658)) AS SIGNED) * (SELECT (CASE WHEN (SELECT (SELECT 0 FROM tbl_16 AS inn_661 LIMIT 1) FROM tbl_20 AS sub_660 LIMIT 1) THEN FALSE ELSE ('alpha_beta_gamma' * (SELECT sub_662.ref_tbl_09_id FROM tbl_21 AS sub_662 LIMIT 1)) END) FROM tbl_17 AS sub_659 LIMIT 1))) AS CHAR) END))) FROM tbl_11 AS sub_629 LIMIT 1)) / NULLIF(t_ref_524.ref_tbl_27_id, 0)) * (t_ref_524.col_3 IN (SELECT sub_663.col_4 FROM tbl_20 AS sub_663)))) AS c_664
FROM tbl_28 AS t_ref_524
WHERE ABS((COALESCE(CAST((CASE WHEN t_ref_524.col_3 THEN ((SELECT COALESCE(CAST(sub_665.col_1 AS CHAR), (SELECT EXISTS (SELECT 1 FROM tbl_04 AS sub_667 WHERE sub_667.col_3) FROM tbl_30 AS sub_666 LIMIT 1)) FROM tbl_14 AS sub_665 LIMIT 1) OR 1e10) ELSE ((EXISTS (SELECT 1 FROM tbl_29 AS sub_669 WHERE sub_669.id) < CAST(((((t_ref_524.col_2 + t_ref_524.id) BETWEEN 2147483647 AND 2147483647) = ((t_ref_524.ref_tbl_27_id BETWEEN -1 AND 2147483647) > (SELECT (SELECT inn_680.col_1 FROM tbl_15 AS inn_680 LIMIT 1) FROM tbl_08 AS sub_679 LIMIT 1))) = CAST((t_ref_524.col_3 IN (SELECT sub_681.id FROM tbl_17 AS sub_681)) AS DECIMAL(10,2))) AS SIGNED)) - 0.0000000001) END) AS CHAR), t_ref_524.id) - EXISTS (SELECT 1 FROM tbl_13 AS sub_682 WHERE sub_682.col_1)))
LIMIT 3;

SELECT
    ((FALSE / NULLIF((SELECT (SELECT inn_697.col_1 FROM tbl_05 AS inn_697 LIMIT 1) FROM tbl_10 AS sub_696 LIMIT 1), 0)) <= CAST(EXISTS (SELECT 1 FROM tbl_28 AS sub_698 WHERE sub_698.ref_tbl_27_id) AS SIGNED)) AS c_709,
    (t_ref_695.col_1 BETWEEN 100 AND -2147483648) AS c_710,
    t_ref_695.id AS c_711
FROM tbl_17 AS t_ref_695
WHERE (t_ref_695.id IN (SELECT sub_712.col_4 FROM tbl_09 AS sub_712))
LIMIT 30;

INSERT INTO tbl_25 (id, col_1, col_2) VALUES (696760, -2147483648, -2147483648);

ALTER TABLE tbl_28 DROP COLUMN col_1;

SELECT
    LENGTH((TRUE > (((SELECT ((CASE WHEN (sub_714.col_2 = CAST((sub_714.col_2 - FALSE) AS CHAR)) THEN sub_714.ref_tbl_02_id ELSE (SELECT (SELECT inn_717.col_2 FROM tbl_30 AS inn_717 LIMIT 1) FROM tbl_26 AS sub_715 LIMIT 1) END) / NULLIF(sub_714.col_2, 0)) FROM tbl_03 AS sub_714 LIMIT 1) - ((NOT (FALSE)) OR CAST(CAST((CASE WHEN ((((COALESCE(TRUE, CAST((SELECT (SELECT inn_721.id FROM tbl_08 AS inn_721 LIMIT 1) FROM tbl_25 AS sub_718 LIMIT 1) AS DECIMAL(10,2))) - CAST((NULLIF(NULLIF(FALSE, (SELECT '' FROM tbl_04 AS sub_722 LIMIT 1)), ((t_ref_713.id * TRUE) = (SELECT (SELECT inn_724.payload FROM invoices AS inn_724 LIMIT 1) FROM tbl_26 AS sub_723 LIMIT 1))) + (((SELECT sub_725.id FROM tbl_30 AS sub_725 LIMIT 1) - (NULL = t_ref_713.id)) AND (CASE WHEN EXISTS (SELECT 1 FROM tbl_07 AS sub_726 WHERE sub_726.dyn_col_4) THEN NULLIF(FALSE, t_ref_713.col_1) ELSE 'alpha_beta_gamma' END))) AS SIGNED)) * NULL) <> ((CAST(LENGTH((NOT (((CASE WHEN t_ref_713.col_3 THEN 42 ELSE t_ref_713.col_2 END) BETWEEN 100 AND -2147483648)))) AS CHAR) <> t_ref_713.id) * ((t_ref_713.col_1 IS NOT NULL) - COALESCE(((CASE WHEN TRUE THEN (CAST(FALSE AS SIGNED) / NULLIF(UPPER(t_ref_713.col_3), 0)) ELSE t_ref_713.col_3 END) / NULLIF(ABS(t_ref_713.col_1), 0)), (SELECT ((FALSE + TRUE) - (sub_727.col_4 * sub_727.col_1)) FROM tbl_18 AS sub_727 LIMIT 1))))) - EXISTS (SELECT 1 FROM tbl_27 AS sub_728 WHERE sub_728.id)) THEN CAST(((CASE WHEN (SELECT (SELECT 2147483647 FROM tbl_04 AS inn_730 LIMIT 1) FROM tbl_10 AS sub_729 LIMIT 1) THEN (NOT (EXISTS (SELECT 1 FROM tbl_02 AS sub_731 WHERE sub_731.ref_tbl_01_id))) ELSE (SELECT (CASE WHEN CAST(((CASE WHEN -9223372036854775808 THEN sub_735.col_1 ELSE '' END) AND ABS(sub_735.id)) AS CHAR) THEN (COALESCE(LOWER(sub_735.col_1), (CASE WHEN NULL THEN sub_735.id ELSE sub_735.col_2 END)) <> (SELECT (SELECT inn_737.col_3 FROM tbl_04 AS inn_737 LIMIT 1) FROM tbl_06 AS sub_736 LIMIT 1)) ELSE (TRUE IN (SELECT sub_738.id FROM invoices AS sub_738)) END) FROM tbl_07 AS sub_735 LIMIT 1) END) * (SELECT sub_739.id FROM tbl_23 AS sub_739 LIMIT 1)) AS DECIMAL(10,2)) ELSE (CASE WHEN (0.0000000001 * CAST(t_ref_713.col_1 AS DECIMAL(10,2))) THEN ((CASE WHEN (EXISTS (SELECT 1 FROM tbl_29 AS sub_740 WHERE sub_740.col_3) <> EXISTS (SELECT 1 FROM tbl_15 AS sub_744 WHERE sub_744.id)) THEN ABS(NULLIF(((SELECT EXISTS (SELECT 1 FROM invoices AS sub_746 WHERE sub_746.payload) FROM tbl_22 AS sub_745 LIMIT 1) = t_ref_713.id), (1e10 IN (SELECT sub_747.id FROM tbl_27 AS sub_747)))) ELSE ((FALSE IS NULL) - ((CASE WHEN EXISTS (SELECT 1 FROM tbl_17 AS sub_748 WHERE sub_748.col_1) THEN (((t_ref_713.id + t_ref_713.col_2) + (t_ref_713.id <> NULL)) + COALESCE(EXISTS (SELECT 1 FROM tbl_19 AS sub_749 WHERE sub_749.col_3), FALSE)) ELSE (NOT (EXISTS (SELECT 1 FROM tbl_20 AS sub_750 WHERE 'escaped''quote'))) END) <> (SELECT (SELECT inn_752.col_1 FROM tbl_19 AS inn_752 LIMIT 1) FROM tbl_27 AS sub_751 LIMIT 1))) END) <= t_ref_713.col_2) ELSE (SELECT (SELECT inn_754.id FROM tbl_11 AS inn_754 LIMIT 1) FROM tbl_17 AS sub_753 LIMIT 1) END) END) AS SIGNED) AS SIGNED))) >= (t_ref_713.col_2 * LENGTH(EXISTS (SELECT 1 FROM tbl_15 AS sub_755 WHERE NULL)))))) AS c_764
FROM tbl_12 AS t_ref_713 LEFT JOIN tbl_09 AS j_ref_765 ON t_ref_713.id = j_ref_765.id
WHERE t_ref_713.col_3
LIMIT 35;

INSERT INTO tbl_25 (id, col_1, col_2) VALUES (112298, 2147483647, 0);

DELETE FROM tbl_22 WHERE id = 28355;

UPDATE tbl_22 SET col_1 = -2147483648 WHERE id > 55;

DELETE FROM tbl_12 WHERE id = 28928;

CREATE TABLE invoices (
    invoice_id INT PRIMARY KEY,
    region_id INT NOT NULL,
    sales_rep_id INT NOT NULL,
    invoice_amount DECIMAL(10,2) NOT NULL
);

SELECT
    i.region_id,
    i.sales_rep_id,
    SUM(i.invoice_amount) AS total_sales
FROM invoices i
WHERE i.region_id IN (
    SELECT d.region_id
    FROM invoice_discounts d
    WHERE d.min_qualifying_amount <= i.invoice_amount
)
GROUP BY i.region_id, i.sales_rep_id WITH ROLLUP
HAVING total_sales > 5000.00;

UPDATE tbl_31 SET col_1 = 'alpha_beta_gamma' WHERE id > 100;

CREATE VIEW v_summary_6 AS SELECT id FROM tbl_07 WHERE id IS NOT NULL;

CREATE TABLE tbl_32 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 INT,
    col_4 NUMERIC(10,2)
);

INSERT INTO tbl_16 (id, col_1, col_2) VALUES (823060, 1e10, 1.0);

SELECT
    t_ref_766.col_1 AS c_767,
    CAST((NOT ('escaped''quote')) AS DECIMAL(10,2)) AS c_768,
    EXISTS (SELECT 1 FROM tbl_18 AS sub_769 WHERE sub_769.col_2) AS c_778
FROM tbl_08 AS t_ref_766 LEFT JOIN tbl_26 AS j_ref_779 ON t_ref_766.id = j_ref_779.id
WHERE CAST(((SELECT sub_780.col_1 FROM tbl_15 AS sub_780 LIMIT 1) <> t_ref_766.col_2) AS CHAR)
LIMIT 17;

CREATE VIEW v_summary_7 AS SELECT id FROM tbl_17 WHERE id IS NOT NULL;

INSERT INTO tbl_25 (id, col_1, col_2) VALUES (698009, 0, 42);

INSERT INTO tbl_18 (id, col_1, col_2, col_3, col_4, ref_tbl_11_id) VALUES (744179, 'alpha_beta_gamma', 'chaos', 1.0, 100, 0);

UPDATE invoices SET payload = 'escaped''quote' WHERE id > 82;

INSERT INTO tbl_06 (id, col_1, col_2, col_3) VALUES (857944, FALSE, 0.0000000001, '{}');

UPDATE tbl_28 SET col_2 = -9223372036854775808 WHERE id > 68;

CREATE TABLE tbl_33 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 BOOLEAN,
    col_4 BOOLEAN
);

SELECT
    ((t_ref_781.id - t_ref_781.id) > ((CAST((((t_ref_781.col_2 <> ((999999999.99 IN (SELECT sub_782.col_3 FROM tbl_29 AS sub_782)) <= (SELECT (SELECT inn_788.id FROM tbl_32 AS inn_788 LIMIT 1) FROM tbl_14 AS sub_787 LIMIT 1))) + (SELECT ((NULL / NULLIF((1 IN (SELECT sub_790.col_4 FROM tbl_20 AS sub_790)), 0)) BETWEEN 100 AND -2147483648) FROM tbl_01 AS sub_789 LIMIT 1)) / NULLIF(ABS('escaped''quote'), 0)) AS CHAR) = (SELECT (SELECT FALSE FROM tbl_09 AS inn_800 LIMIT 1) FROM tbl_08 AS sub_793 LIMIT 1)) / NULLIF((NULLIF(t_ref_781.ref_tbl_14_id, (t_ref_781.id <= (CAST((t_ref_781.col_2 = (NULLIF(COALESCE((((SELECT (SELECT inn_802.col_1 FROM tbl_27 AS inn_802 LIMIT 1) FROM tbl_08 AS sub_801 LIMIT 1) BETWEEN 0 AND -9223372036854775808) - (((CAST(t_ref_781.col_1 AS SIGNED) * NULL) < t_ref_781.col_2) + ((CASE WHEN NULLIF(t_ref_781.ref_tbl_14_id, (CASE WHEN 'ref_val' THEN t_ref_781.col_1 ELSE t_ref_781.col_2 END)) THEN 'alpha_beta_gamma' ELSE (CASE WHEN t_ref_781.col_1 THEN (t_ref_781.id - t_ref_781.ref_tbl_14_id) ELSE (t_ref_781.id OR t_ref_781.ref_tbl_14_id) END) END) = -1))), ((t_ref_781.ref_tbl_14_id IN (SELECT sub_803.id FROM tbl_22 AS sub_803)) > -1e-5)), (t_ref_781.col_1 IN (SELECT sub_805.col_1 FROM tbl_26 AS sub_805))) * CAST((-1 - EXISTS (SELECT 1 FROM tbl_14 AS sub_808 WHERE 999999999.99)) AS CHAR))) AS SIGNED) - CAST(-1.5 AS CHAR)))) OR ((CAST(LENGTH(CAST((SELECT ((EXISTS (SELECT 1 FROM tbl_03 AS sub_811 WHERE sub_811.col_2) + (FALSE IN (SELECT sub_812.col_1 FROM tbl_17 AS sub_812))) + (NOT (1e10))) FROM tbl_25 AS sub_810 LIMIT 1) AS SIGNED)) AS SIGNED) = (NULL - t_ref_781.id)) < (NULLIF((SELECT CAST((sub_813.id IN (SELECT sub_814.id FROM tbl_28 AS sub_814)) AS SIGNED) FROM tbl_15 AS sub_813 LIMIT 1), (CAST((EXISTS (SELECT 1 FROM tbl_17 AS sub_820 WHERE NULL) - (SELECT sub_821.col_2 FROM tbl_08 AS sub_821 LIMIT 1)) AS CHAR) - t_ref_781.col_2)) <= (((CASE WHEN (((LOWER((CASE WHEN (CASE WHEN (t_ref_781.col_2 IN (SELECT sub_822.id FROM tbl_13 AS sub_822)) THEN 1.0 ELSE LOWER((CAST(t_ref_781.id AS SIGNED) + (SELECT (SELECT inn_824.col_2 FROM tbl_15 AS inn_824 LIMIT 1) FROM tbl_16 AS sub_823 LIMIT 1))) END) THEN ((EXISTS (SELECT 1 FROM tbl_17 AS sub_825 WHERE 0) <= t_ref_781.col_2) IS NULL) ELSE 0 END)) / NULLIF(EXISTS (SELECT 1 FROM tbl_12 AS sub_826 WHERE sub_826.col_2), 0)) - t_ref_781.col_2) / NULLIF(t_ref_781.id, 0)) THEN (SELECT (SELECT FALSE FROM tbl_03 AS inn_830 LIMIT 1) FROM tbl_15 AS sub_828 LIMIT 1) ELSE (NULL IN (SELECT sub_831.id FROM tbl_27 AS sub_831)) END) AND (TRUE + LENGTH(t_ref_781.col_2))) < NULLIF((SELECT (SELECT inn_835.col_1 FROM tbl_19 AS inn_835 LIMIT 1) FROM tbl_28 AS sub_833 LIMIT 1), (NULLIF(((ABS(t_ref_781.col_2) AND EXISTS (SELECT 1 FROM tbl_06 AS sub_836 WHERE sub_836.id)) - (t_ref_781.id IN (SELECT sub_839.col_2 FROM tbl_04 AS sub_839))), ABS(NULLIF(TRUE, (SELECT (SELECT inn_841.ref_tbl_12_id FROM tbl_26 AS inn_841 LIMIT 1) FROM tbl_32 AS sub_840 LIMIT 1)))) * EXISTS (SELECT 1 FROM tbl_25 AS sub_842 WHERE '__fuzz_marker__'))))))), 0))) AS c_844,
    (t_ref_781.ref_tbl_14_id OR CAST((((t_ref_781.col_2 IN (SELECT sub_845.col_2 FROM tbl_11 AS sub_845)) - t_ref_781.id) >= (SELECT (SELECT inn_847.id FROM tbl_22 AS inn_847 LIMIT 1) FROM tbl_08 AS sub_846 LIMIT 1)) AS DECIMAL(10,2))) AS c_848
FROM tbl_27 AS t_ref_781 LEFT JOIN tbl_03 AS j_ref_849 ON t_ref_781.id = j_ref_849.id
WHERE ((-1.5 IN (SELECT sub_850.id FROM tbl_27 AS sub_850)) * t_ref_781.ref_tbl_14_id)
LIMIT 21;

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (624434, '[]', '{}', '', 'escaped''quote', -9223372036854775808);

ALTER TABLE tbl_30 ADD COLUMN dyn_col_6 VARCHAR(32);

INSERT INTO tbl_33 (id, col_1, col_2, col_3, col_4) VALUES (858839, -2147483648, 1e10, FALSE, TRUE);

INSERT INTO tbl_03 (id, col_1, col_2, ref_tbl_02_id) VALUES (300124, '{}', FALSE, 0);

UPDATE tbl_07 SET col_1 = '{}' WHERE id > 30;

UPDATE tbl_14 SET col_2 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 21;

CREATE TABLE tbl_34 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSON,
    col_3 JSON,
    ref_tbl_30_id INT,
    FOREIGN KEY (ref_tbl_30_id) REFERENCES tbl_30(id)
);

CREATE VIEW v_summary_8 AS SELECT id FROM tbl_16 WHERE id IS NOT NULL;

DELETE FROM tbl_03 WHERE id = 8585;

CREATE VIEW v_summary_9 AS SELECT id FROM tbl_18 WHERE id IS NOT NULL;

CREATE INDEX idx_tbl_32_col_2_9 ON tbl_32 (col_2);

UPDATE tbl_21 SET ref_tbl_09_id = -9223372036854775808 WHERE id > 62;

SELECT
    t_ref_851.col_2 AS c_852,
    (((SELECT EXISTS (SELECT 1 FROM tbl_26 AS sub_854 WHERE NULL) FROM tbl_11 AS sub_853 LIMIT 1) - 0.0000000001) > (SELECT (SELECT inn_877.col_3 FROM tbl_18 AS inn_877 LIMIT 1) FROM tbl_12 AS sub_872 LIMIT 1)) AS c_878
FROM tbl_10 AS t_ref_851
WHERE (CASE WHEN (LENGTH(TRUE) OR ABS(t_ref_851.col_1)) THEN (NULL + CAST(9223372036854775807 AS SIGNED)) ELSE ((-9223372036854775808 / NULLIF(COALESCE((SELECT ABS(((SELECT (SELECT -9223372036854775808 FROM tbl_17 AS inn_883 LIMIT 1) FROM tbl_15 AS sub_880 LIMIT 1) <= ABS(((sub_879.id = ((SELECT (CASE WHEN (CASE WHEN (TRUE AND sub_884.id) THEN ('__fuzz_marker__' IS NOT NULL) ELSE FALSE END) THEN sub_884.id ELSE ABS(sub_884.id) END) FROM tbl_17 AS sub_884 LIMIT 1) / NULLIF(((SELECT (SELECT inn_887.col_1 FROM tbl_32 AS inn_887 LIMIT 1) FROM tbl_21 AS sub_885 LIMIT 1) <> sub_879.id), 0))) <= LENGTH(((LENGTH(CAST(sub_879.id AS DECIMAL(10,2))) / NULLIF(sub_879.col_1, 0)) >= ((((sub_879.col_1 > (CASE WHEN sub_879.col_1 THEN TRUE ELSE -1.5 END)) <= sub_879.id) - sub_879.id) - (SELECT (SELECT FALSE FROM tbl_27 AS inn_890 LIMIT 1) FROM tbl_30 AS sub_888 LIMIT 1)))))))) FROM tbl_17 AS sub_879 LIMIT 1), t_ref_851.col_3), 0)) / NULLIF(EXISTS (SELECT 1 FROM tbl_14 AS sub_891 WHERE sub_891.col_2), 0)) END)
LIMIT 6;

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (961919, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', -1e-5, 'chaos');

CREATE VIEW v_summary_10 AS SELECT id FROM tbl_23 WHERE id IS NOT NULL;
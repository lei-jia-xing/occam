-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: MYSQL

-- Statements: 50

-- AST Expression Depth: 12

-- Injected Defect Case: mysql_03

-- Seed: 5022



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 INT,
    col_4 VARCHAR(64)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 INT,
    col_3 NUMERIC(10,2),
    col_4 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 NUMERIC(10,2),
    col_3 JSON
);

UPDATE tbl_01 SET col_4 = 'chaos' WHERE id > 22;

CREATE INDEX idx_tbl_02_ref_tbl_01_id_1 ON tbl_02 (ref_tbl_01_id);

CREATE TABLE doc_registry (
    doc_id INT PRIMARY KEY,
    tenant_id INT NOT NULL,
    payload JSON NOT NULL
);

ALTER TABLE tbl_01 ADD COLUMN dyn_col_6 VARCHAR(32);

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6) VALUES (259683, 42, 'chaos', 42, 'escaped''quote', '');
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (750335, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 42, -1.5, 'ref_val', 42);
INSERT INTO tbl_03 (id, col_1, col_2, col_3) VALUES (428491, 'chaos', 0.0, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

SELECT
    CAST((CASE WHEN t_ref_1.col_2 THEN CAST(NULL AS CHAR) ELSE t_ref_1.col_1 END) AS DECIMAL(10,2)) AS c_2
FROM tbl_02 AS t_ref_1 LEFT JOIN tbl_03 AS j_ref_3 ON t_ref_1.id = j_ref_3.id
WHERE (t_ref_1.col_2 IN (SELECT sub_4.col_3 FROM tbl_02 AS sub_4))
LIMIT 40;

ALTER TABLE tbl_01 ADD COLUMN dyn_col_7 VARCHAR(32);

CREATE VIEW v_summary_1 AS SELECT id FROM doc_registry WHERE id IS NOT NULL;

INSERT INTO tbl_03 (id, col_1, col_2, col_3) VALUES (628053, 'alpha_beta_gamma', 0.0, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

INSERT INTO tbl_03 (id, col_1, col_2, col_3) VALUES (556011, 'escaped''quote', 999999999.99, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

CREATE INDEX idx_tbl_03_col_1_2 ON tbl_03 (col_1);

BEGIN;
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6, dyn_col_7) VALUES (243403, 42, '__fuzz_marker__', 100, 'ref_val', '', 'chaos');
INSERT INTO tbl_03 (id, col_1, col_2, col_3) VALUES (589480, '', 3.14159265, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
INSERT INTO tbl_03 (id, col_1, col_2, col_3) VALUES (256565, 'alpha_beta_gamma', 0.0, '{}');
COMMIT;

INSERT INTO doc_registry (doc_id, tenant_id, payload) VALUES
    (1, 100, '{"departments": [{"name": "eng", "roles": [{"title": "dev", "clearance": 3}, {"title": "qa", "clearance": 2}]}, {"name": "hr", "roles": [{"title": "recruiter", "clearance": 1}]}]}'),
    (2, 100, '{"departments": [{"name": "finance", "roles": []}, {"name": "legal", "roles": null}]}'),
    (3, 200, '{"departments": "not_an_array", "extra": [1, 2, [3, 4]]}'),
    (4, 200, '{"departments": [{"roles": [{"title": "exec"}]}]}');

UPDATE tbl_03 SET col_3 = '{}' WHERE id > 44;

UPDATE tbl_03 SET col_1 = '__fuzz_marker__' WHERE id > 56;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (99757, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', 2147483647, 999999999.99, '__fuzz_marker__', 1);

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6, dyn_col_7) VALUES (57963, -9223372036854775808, 'alpha_beta_gamma', 1, '', 'chaos', 'escaped''quote');

UPDATE tbl_02 SET col_2 = -2147483648 WHERE id > 21;

SELECT
    (t_ref_21.col_3 * (EXISTS (SELECT 1 FROM doc_registry AS sub_22 WHERE sub_22.id) * t_ref_21.id)) AS c_23,
    (NOT (t_ref_21.col_2)) AS c_24,
    (SELECT ((CAST((EXISTS (SELECT 1 FROM tbl_03 AS sub_26 WHERE sub_26.col_3) * (NOT ((SELECT LENGTH((sub_28.id <= sub_28.id)) FROM doc_registry AS sub_28 LIMIT 1)))) AS CHAR) <> (UPPER(((COALESCE((SELECT sub_29.id FROM tbl_03 AS sub_29 LIMIT 1), sub_25.id) <> (((sub_25.id OR sub_25.col_1) OR CAST(sub_25.col_2 AS CHAR)) + sub_25.id)) BETWEEN -2147483648 AND -2147483648)) - 0.0000000001)) / NULLIF(((NOT (ABS((NOT (CAST((SELECT (sub_30.id - -1e-5) FROM tbl_02 AS sub_30 LIMIT 1) AS SIGNED)))))) = (TRUE AND (sub_25.col_2 < (ABS(UPPER((TRUE <= -1e-5))) - (NOT ((SELECT (SELECT inn_32.payload FROM doc_registry AS inn_32 LIMIT 1) FROM tbl_01 AS sub_31 LIMIT 1))))))), 0)) FROM tbl_03 AS sub_25 LIMIT 1) AS c_33
FROM tbl_03 AS t_ref_21
WHERE COALESCE(NULLIF((t_ref_21.col_3 IN (SELECT sub_34.id FROM doc_registry AS sub_34)), (SELECT (SELECT inn_39.col_4 FROM tbl_02 AS inn_39 LIMIT 1) FROM tbl_03 AS sub_37 LIMIT 1)), ((((((SELECT (SELECT -2147483648 FROM tbl_03 AS inn_41 LIMIT 1) FROM tbl_03 AS sub_40 LIMIT 1) <= t_ref_21.col_1) OR (NULL IN (SELECT sub_42.dyn_col_6 FROM tbl_01 AS sub_42))) < (((CAST((SELECT (SELECT inn_44.dyn_col_7 FROM tbl_01 AS inn_44 LIMIT 1) FROM tbl_03 AS sub_43 LIMIT 1) AS CHAR) >= COALESCE(CAST((SELECT 0.0000000001 FROM tbl_03 AS sub_45 LIMIT 1) AS CHAR), ((NOT (CAST(t_ref_21.col_3 AS CHAR))) = (LENGTH(FALSE) = NULLIF(TRUE, t_ref_21.col_3))))) + (NULLIF(FALSE, NULLIF((t_ref_21.col_2 AND (SELECT (SELECT inn_47.col_2 FROM tbl_02 AS inn_47 LIMIT 1) FROM tbl_03 AS sub_46 LIMIT 1)), UPPER((t_ref_21.col_1 IN (SELECT sub_48.ref_tbl_01_id FROM tbl_02 AS sub_48))))) / NULLIF(-1e-5, 0))) > EXISTS (SELECT 1 FROM tbl_02 AS sub_49 WHERE sub_49.col_4))) = t_ref_21.col_3) = ((((CAST((t_ref_21.col_1 IS NOT NULL) AS CHAR) * (FALSE IN (SELECT sub_51.col_2 FROM tbl_02 AS sub_51))) / NULLIF(((COALESCE((t_ref_21.col_1 * (CASE WHEN (CASE WHEN t_ref_21.col_1 THEN t_ref_21.col_1 ELSE t_ref_21.id END) THEN t_ref_21.col_1 ELSE t_ref_21.col_1 END)), (((t_ref_21.col_3 > t_ref_21.col_3) = (CASE WHEN t_ref_21.id THEN TRUE ELSE t_ref_21.id END)) <> (SELECT (SELECT inn_53.payload FROM doc_registry AS inn_53 LIMIT 1) FROM tbl_01 AS sub_52 LIMIT 1))) + COALESCE(FALSE, 100)) < EXISTS (SELECT 1 FROM tbl_02 AS sub_54 WHERE TRUE)), 0)) + (t_ref_21.col_2 <= (t_ref_21.col_1 IN (SELECT sub_55.col_1 FROM tbl_02 AS sub_55)))) BETWEEN 100 AND 2147483647)))
LIMIT 18;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (684442, '[]', 1, 1.0, '', 0);

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6, dyn_col_7) VALUES (216805, 9223372036854775807, 'escaped''quote', 9223372036854775807, 'escaped''quote', '', 'alpha_beta_gamma');

UPDATE doc_registry SET payload = 'alpha_beta_gamma' WHERE id > 18;

INSERT INTO doc_registry (id, payload) VALUES (798894, '__fuzz_marker__');

INSERT INTO doc_registry (id, payload) VALUES (789, 'chaos');

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_01 WHERE id IS NOT NULL;

INSERT INTO tbl_03 (id, col_1, col_2, col_3) VALUES (981004, 'escaped''quote', 0.0, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 INT
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 JSON,
    col_4 JSON,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6, dyn_col_7) VALUES (111961, -2147483648, '', 1, 'alpha_beta_gamma', 'chaos', 'chaos');

DELETE FROM tbl_03 WHERE id = 46665;

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 JSON,
    col_2 JSON
);

ALTER TABLE tbl_05 DROP COLUMN ref_tbl_01_id;

CREATE INDEX idx_tbl_05_col_4_3 ON tbl_05 (col_4);

BEGIN;
INSERT INTO doc_registry (id, payload) VALUES (5223, '');
COMMIT;

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_01 WHERE id IS NOT NULL;

CREATE TABLE doc_registry (
    doc_id INT PRIMARY KEY,
    tenant_id INT NOT NULL,
    payload JSON NOT NULL
);

SELECT
    doc_id,
    JSON_EXTRACT(payload, '$.departments[*].roles[*].title') AS extracted_titles,
    JSON_UNQUOTE(JSON_EXTRACT(payload, '$.departments[0].name')) AS primary_dept
FROM doc_registry
WHERE JSON_CONTAINS_PATH(payload, 'one', '$.departments[*].roles')
  AND JSON_LENGTH(JSON_EXTRACT(payload, '$.departments[*].roles[*]')) > 0
ORDER BY doc_id;

SELECT
    t_ref_56.payload AS c_57
FROM doc_registry AS t_ref_56 LEFT JOIN tbl_05 AS j_ref_58 ON t_ref_56.id = j_ref_58.id
WHERE (UPPER(CAST(t_ref_56.id AS DECIMAL(10,2))) <> LOWER((((((((CASE WHEN CAST((t_ref_56.payload + FALSE) AS CHAR) THEN (ABS(-1.5) + (t_ref_56.id IS NOT NULL)) ELSE t_ref_56.payload END) BETWEEN -2147483648 AND 2147483647) / NULLIF('', 0)) / NULLIF(((t_ref_56.id IN (SELECT sub_59.col_1 FROM tbl_06 AS sub_59)) > (ABS((t_ref_56.payload IN (SELECT sub_61.col_4 FROM tbl_02 AS sub_61))) - (SELECT (sub_62.id + sub_62.col_2) FROM tbl_02 AS sub_62 LIMIT 1))), 0)) > (t_ref_56.payload * (EXISTS (SELECT 1 FROM tbl_04 AS sub_63 WHERE sub_63.col_1) IS NULL))) + LOWER(t_ref_56.id)) <= ((UPPER((CASE WHEN (2147483647 * (CAST((t_ref_56.payload IS NOT NULL) AS SIGNED) OR ABS(LOWER(t_ref_56.id)))) THEN (SELECT (SELECT NULL FROM tbl_03 AS inn_65 LIMIT 1) FROM doc_registry AS sub_64 LIMIT 1) ELSE ((1e10 > t_ref_56.payload) / NULLIF((EXISTS (SELECT 1 FROM tbl_02 AS sub_66 WHERE sub_66.col_4) BETWEEN 0 AND 100), 0)) END)) / NULLIF((CASE WHEN EXISTS (SELECT 1 FROM tbl_04 AS sub_67 WHERE 'escaped''quote') THEN (t_ref_56.id + t_ref_56.payload) ELSE ((EXISTS (SELECT 1 FROM tbl_06 AS sub_70 WHERE sub_70.col_2) * CAST(((SELECT FALSE FROM tbl_05 AS sub_71 LIMIT 1) * t_ref_56.payload) AS DECIMAL(10,2))) <> (CAST(((t_ref_56.payload >= t_ref_56.payload) * NULLIF(t_ref_56.id, TRUE)) AS DECIMAL(10,2)) * (SELECT (SELECT (SELECT inn_74.dyn_col_7 FROM tbl_01 AS inn_74 LIMIT 1) FROM tbl_06 AS sub_73 LIMIT 1) FROM tbl_03 AS sub_72 LIMIT 1))) END), 0)) + (CASE WHEN EXISTS (SELECT 1 FROM tbl_02 AS sub_75 WHERE sub_75.col_3) THEN (t_ref_56.payload * CAST(t_ref_56.payload AS DECIMAL(10,2))) ELSE (1.0 IS NOT NULL) END)))))
LIMIT 5;

CREATE INDEX idx_tbl_04_col_1_4 ON tbl_04 (col_1);

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6, dyn_col_7) VALUES (387488, 9223372036854775807, 'ref_val', 100, '__fuzz_marker__', 'chaos', '');

UPDATE tbl_01 SET col_4 = 'chaos' WHERE id > 46;

CREATE INDEX idx_tbl_05_id_5 ON tbl_05 (id);

UPDATE doc_registry SET payload = 'chaos' WHERE id > 71;

UPDATE tbl_03 SET col_3 = '{"ref_id": 101, "payload": {"nested": {"v": -1}}}' WHERE id > 3;

BEGIN;
INSERT INTO doc_registry (id, payload) VALUES (196671, 'escaped''quote');
INSERT INTO tbl_02 (id, col_1, col_2, col_3, col_4, ref_tbl_01_id) VALUES (272689, '[]', 100, 3.14159265, '__fuzz_marker__', 42);
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4, dyn_col_6, dyn_col_7) VALUES (629978, 2147483647, 'chaos', 0, 'ref_val', 'ref_val', '');
COMMIT;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 VARCHAR(64),
    col_3 VARCHAR(64),
    col_4 NUMERIC(10,2),
    ref_tbl_06_id INT,
    FOREIGN KEY (ref_tbl_06_id) REFERENCES tbl_06(id)
);
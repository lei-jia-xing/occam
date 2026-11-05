-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: POSTGRES

-- Statements: 50

-- AST Expression Depth: 10

-- Injected Defect Case: pg_01

-- Seed: 5011



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSONB,
    col_3 VARCHAR(64),
    col_4 VARCHAR(64)
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 VARCHAR(64),
    col_3 INT,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64),
    col_3 JSONB,
    col_4 INT
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 VARCHAR(64),
    col_3 VARCHAR(64),
    col_4 VARCHAR(64)
);

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 BOOLEAN,
    col_3 VARCHAR(64)
);

CREATE TABLE customer_profiles (
    profile_id INT PRIMARY KEY,
    tenant_id INT NOT NULL,
    account_status VARCHAR(24),
    profile_doc JSONB NOT NULL
);

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 BOOLEAN,
    col_3 INT,
    ref_tbl_02_id INT,
    FOREIGN KEY (ref_tbl_02_id) REFERENCES tbl_02(id)
);

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (89758, 1.0, '[]', 'ref_val', '');

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 BOOLEAN,
    ref_tbl_04_id INT,
    FOREIGN KEY (ref_tbl_04_id) REFERENCES tbl_04(id)
);

CREATE INDEX idx_customer_profiles_doc_gin ON customer_profiles USING gin (profile_doc);

DELETE FROM tbl_07 WHERE id = 89328;

CREATE VIEW v_summary_2 AS SELECT id FROM customer_profiles WHERE id IS NOT NULL;

INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (976457, 'alpha_beta_gamma', 'escaped''quote', '', 'ref_val');

BEGIN;
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (365195, TRUE, '__fuzz_marker__', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 1);
INSERT INTO tbl_06 (id, col_1, col_2, col_3, ref_tbl_02_id) VALUES (13623, '[]', TRUE, 100, -1);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (511297, TRUE, '__fuzz_marker__', '{}', -2147483648);
COMMIT;

INSERT INTO customer_profiles (profile_id, tenant_id, account_status, profile_doc) VALUES
    (1, 10, 'active', '{"user": {"name": "Alice", "contact": {"emails": [{"type": "work", "address": "alice@corp.com"}, {"type": "home", "address": null}]}}, "settings": {"dark_mode": true}}'),
    (2, 10, 'suspended', '{"user": {"name": "Bob", "contact": {"emails": []}}, "metadata": null}'),
    (3, 20, 'active', '{"user": {"name": "Charlie", "contact": null}, "tags": ["beta", "internal"]}'),
    (4, 20, 'pending', '{"user": null, "notes": "unverified registration"}');

SELECT
    (t_ref_1.col_3 + ((CASE WHEN ((t_ref_1.col_2 IN (SELECT sub_2.payload FROM customer_profiles AS sub_2)) - ((((EXISTS (SELECT 1 FROM tbl_03 AS sub_5 WHERE sub_5.col_2) + ('ref_val' / NULLIF(TRUE, 0)))::INT) = EXISTS (SELECT 1 FROM tbl_07 AS sub_6 WHERE sub_6.col_1)) + ((SELECT (SELECT inn_8.id FROM tbl_02 AS inn_8 LIMIT 1) FROM customer_profiles AS sub_7 LIMIT 1) - (NOT ((UPPER(2147483647) * 100)))))) THEN (CASE WHEN t_ref_1.ref_tbl_01_id THEN COALESCE(ABS((((TRUE - 'alpha_beta_gamma') / NULLIF(t_ref_1.col_3, 0)) - ((SELECT (SELECT 'alpha_beta_gamma' FROM tbl_03 AS inn_10 LIMIT 1) FROM tbl_01 AS sub_9 LIMIT 1) / NULLIF((FALSE::BOOLEAN), 0)))), EXISTS (SELECT 1 FROM tbl_07 AS sub_11 WHERE sub_11.col_2)) ELSE (((0 IN (SELECT sub_13.col_2 FROM tbl_05 AS sub_13)) / NULLIF(((('__fuzz_marker__'::NUMERIC(10,2)) * (CASE WHEN t_ref_1.id THEN -9223372036854775808 ELSE -1e-5 END))::TEXT), 0)) < (((2147483647 <> (t_ref_1.ref_tbl_01_id::NUMERIC(10,2))) BETWEEN 1 AND -9223372036854775808) * 'ref_val')) END) ELSE ((TRUE <= ((((CASE WHEN t_ref_1.ref_tbl_01_id THEN -1e-5 ELSE 'ref_val' END) >= ABS(t_ref_1.col_1)) - EXISTS (SELECT 1 FROM tbl_03 AS sub_14 WHERE sub_14.col_4))::BOOLEAN)) * EXISTS (SELECT 1 FROM tbl_07 AS sub_15 WHERE sub_15.ref_tbl_04_id)) END)::INT::TEXT)) AS c_16
FROM tbl_02 AS t_ref_1
WHERE EXISTS (SELECT 1 FROM tbl_02 AS sub_17 WHERE sub_17.ref_tbl_01_id)
LIMIT 14;

DELETE FROM tbl_05 WHERE id = 38785;

CREATE INDEX idx_tbl_04_id_1 ON tbl_04 (id);

UPDATE tbl_02 SET ref_tbl_01_id = -1 WHERE id > 69;

UPDATE customer_profiles
SET profile_doc = jsonb_set(profile_doc, '{settings,backup_email}', '"backup@corp.com"')
WHERE profile_id = 1;

INSERT INTO tbl_05 (id, col_1, col_2, col_3) VALUES (235082, '[]', TRUE, '__fuzz_marker__');

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2, col_3, ref_tbl_02_id) VALUES (498536, '[]', TRUE, -2147483648, 1);
COMMIT;

INSERT INTO tbl_07 (id, col_1, col_2, col_3, ref_tbl_04_id) VALUES (870279, -1, 0.0000000001, TRUE, 1);

CREATE VIEW v_summary_3 AS SELECT id FROM tbl_06 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO tbl_07 (id, col_1, col_2, col_3, ref_tbl_04_id) VALUES (223779, 42, 999999999.99, TRUE, -2147483648);
INSERT INTO tbl_07 (id, col_1, col_2, col_3, ref_tbl_04_id) VALUES (45109, 9223372036854775807, 999999999.99, TRUE, 100);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, col_4) VALUES (944220, TRUE, '', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 42);
COMMIT;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (640315, 'chaos', 'escaped''quote', 'alpha_beta_gamma', 'chaos');
INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (392803, 0.0000000001, '[]', 'ref_val', 'escaped''quote');
INSERT INTO tbl_06 (id, col_1, col_2, col_3, ref_tbl_02_id) VALUES (771018, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', FALSE, -2147483648, -2147483648);
COMMIT;

INSERT INTO tbl_06 (id, col_1, col_2, col_3, ref_tbl_02_id) VALUES (24635, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', TRUE, 2147483647, 1);

INSERT INTO customer_profiles (id, payload) VALUES (389633, 'alpha_beta_gamma');

SELECT
    (t_ref_18.col_2 IN (SELECT sub_19.id FROM tbl_04 AS sub_19)) AS c_20,
    ((t_ref_18.id - (t_ref_18.col_1 OR (UPPER((((CASE WHEN (CASE WHEN 42 THEN -2147483648 ELSE t_ref_18.col_2 END) THEN (t_ref_18.id + NULL) ELSE LENGTH(t_ref_18.id) END)::TEXT) - t_ref_18.col_2)) / NULLIF((LOWER(UPPER(((t_ref_18.id > -9223372036854775808)::NUMERIC(10,2)))) + 'escaped''quote'), 0)))) - (LENGTH(((NULL::INT) * NULLIF(t_ref_18.col_3, (LOWER(((CASE WHEN 'escaped''quote' THEN t_ref_18.col_1 ELSE 0 END) + t_ref_18.col_3)) AND '__fuzz_marker__')))) BETWEEN 0 AND 42)) AS c_21
FROM tbl_03 AS t_ref_18 LEFT JOIN tbl_01 AS j_ref_22 ON t_ref_18.id = j_ref_22.id
WHERE FALSE
LIMIT 21;

CREATE INDEX idx_tbl_03_col_1_2 ON tbl_03 (col_1);

CREATE INDEX idx_tbl_07_col_2_3 ON tbl_07 (col_2);

UPDATE tbl_07 SET ref_tbl_04_id = -1 WHERE id > 14;

UPDATE tbl_04 SET col_3 = '' WHERE id > 13;

CREATE TABLE customer_profiles (
    profile_id INT PRIMARY KEY,
    tenant_id INT NOT NULL,
    account_status VARCHAR(24),
    profile_doc JSONB NOT NULL
);

SELECT
    p.profile_id,
    p.tenant_id,
    jsonb_strip_nulls(p.profile_doc) AS cleaned_doc,
    jsonb_path_query(p.profile_doc, 'lax $.user.contact.emails[*] ? (@.type == "work").address') AS work_email
FROM customer_profiles p
WHERE p.account_status = 'active'
  AND p.profile_doc @> '{"user": {}}'
ORDER BY p.profile_id;

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 JSONB,
    col_2 JSONB
);

ALTER TABLE tbl_03 ADD COLUMN dyn_col_6 VARCHAR(32);

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 VARCHAR(64)
);

ALTER TABLE tbl_05 DROP COLUMN col_2;

CREATE VIEW v_summary_4 AS SELECT id FROM customer_profiles WHERE id IS NOT NULL;

INSERT INTO tbl_02 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (998966, -1.5, 'chaos', 0, -1);

BEGIN;
INSERT INTO tbl_08 (id, col_1, col_2) VALUES (615081, '{}', '{}');
INSERT INTO tbl_05 (id, col_1, col_3) VALUES (578276, '{}', '__fuzz_marker__');
INSERT INTO tbl_04 (id, col_1, col_2, col_3, col_4) VALUES (139205, 'escaped''quote', '__fuzz_marker__', 'ref_val', 'escaped''quote');
COMMIT;

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 JSONB,
    col_4 INT
);

SELECT
    t_ref_23.col_2 AS c_24,
    (NULL::INT::TEXT) AS c_25,
    (NOT (COALESCE(t_ref_23.col_4, (SELECT sub_26.id FROM tbl_04 AS sub_26 LIMIT 1)))) AS c_27
FROM tbl_01 AS t_ref_23
WHERE ((t_ref_23.col_4 - (((NULLIF(LOWER((NULL - (t_ref_23.col_3 / NULLIF(t_ref_23.id, 0)))), (t_ref_23.col_2 IN (SELECT sub_28.id FROM tbl_08 AS sub_28))) / NULLIF((SELECT ((sub_29.col_1::INT)::INT) FROM tbl_07 AS sub_29 LIMIT 1), 0)) * (NOT (t_ref_23.col_2))) + ((ABS(t_ref_23.id) + ((t_ref_23.id::TEXT) - (t_ref_23.id / NULLIF((t_ref_23.col_1 IN (SELECT sub_30.col_2 FROM tbl_10 AS sub_30)), 0)))) * ((t_ref_23.id IN (SELECT sub_31.col_1 FROM tbl_02 AS sub_31)) + '__fuzz_marker__')))) * ((UPPER((CASE WHEN (SELECT (SELECT inn_33.col_2 FROM tbl_02 AS inn_33 LIMIT 1) FROM tbl_05 AS sub_32 LIMIT 1) THEN (SELECT (SELECT inn_35.id FROM tbl_03 AS inn_35 LIMIT 1) FROM customer_profiles AS sub_34 LIMIT 1) ELSE UPPER((t_ref_23.col_3 IN (SELECT sub_36.col_2 FROM tbl_03 AS sub_36))) END)) * EXISTS (SELECT 1 FROM tbl_10 AS sub_37 WHERE 1.0)) AND (ABS((EXISTS (SELECT 1 FROM tbl_07 AS sub_38 WHERE -1.5) + ((CASE WHEN '__fuzz_marker__' THEN EXISTS (SELECT 1 FROM tbl_04 AS sub_40 WHERE sub_40.id) ELSE LOWER((t_ref_23.col_4 <> 'ref_val')) END)::NUMERIC(10,2))))::INT)))
LIMIT 6;

INSERT INTO tbl_01 (id, col_1, col_2, col_3, col_4) VALUES (165091, -1.5, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 'ref_val', 'escaped''quote');

SELECT
    (((SELECT (SELECT inn_46.col_1 FROM tbl_02 AS inn_46 LIMIT 1) FROM tbl_05 AS sub_42 LIMIT 1) / NULLIF((LOWER(NULL) * UPPER((t_ref_41.col_2 IN (SELECT sub_47.ref_tbl_02_id FROM tbl_06 AS sub_47)))), 0)) >= EXISTS (SELECT 1 FROM tbl_10 AS sub_48 WHERE sub_48.id)) AS c_49,
    (CASE WHEN COALESCE(((CASE WHEN (NOT ((t_ref_41.id IN (SELECT sub_50.id FROM tbl_04 AS sub_50)))) THEN t_ref_41.col_2 ELSE 999999999.99 END) * (SELECT ABS(sub_52.id) FROM customer_profiles AS sub_52 LIMIT 1)), (((t_ref_41.col_1 - ABS(t_ref_41.id)) * t_ref_41.id) + NULLIF(((((EXISTS (SELECT 1 FROM tbl_05 AS sub_53 WHERE sub_53.col_3) AND EXISTS (SELECT 1 FROM tbl_02 AS sub_54 WHERE 1))::TEXT) - UPPER((t_ref_41.col_1 = (t_ref_41.col_1 OR TRUE)))) OR (t_ref_41.col_1 BETWEEN -1 AND -1)), (((t_ref_41.col_1 OR ((t_ref_41.id IS NULL) IS NULL)) * (((NULL * NULL) * (CASE WHEN t_ref_41.id THEN t_ref_41.col_1 ELSE t_ref_41.id END)) + (t_ref_41.id AND (t_ref_41.col_2 + t_ref_41.col_1))))::NUMERIC(10,2))))) THEN t_ref_41.col_2 ELSE (NOT (LENGTH((CASE WHEN EXISTS (SELECT 1 FROM tbl_05 AS sub_55 WHERE 42) THEN (FALSE / NULLIF((SELECT (SELECT inn_59.id FROM customer_profiles AS inn_59 LIMIT 1) FROM tbl_04 AS sub_57 LIMIT 1), 0)) ELSE t_ref_41.col_2 END)))) END) AS c_60
FROM tbl_08 AS t_ref_41 LEFT JOIN tbl_07 AS j_ref_61 ON t_ref_41.id = j_ref_61.id
WHERE ((t_ref_41.col_2 IN (SELECT sub_62.col_3 FROM tbl_04 AS sub_62)) - 1)
LIMIT 37;

CREATE VIEW v_summary_5 AS SELECT id FROM tbl_03 WHERE id IS NOT NULL;

SELECT
    COALESCE((SELECT (SELECT (SELECT 2147483647 FROM tbl_09 AS inn_74 LIMIT 1) FROM tbl_01 AS sub_68 LIMIT 1) FROM tbl_03 AS sub_67 LIMIT 1), t_ref_66.col_1) AS c_75,
    (SELECT (sub_76.col_4 / NULLIF(((sub_76.col_1 AND (SELECT EXISTS (SELECT 1 FROM tbl_06 AS sub_78 WHERE 1) FROM tbl_05 AS sub_77 LIMIT 1)) <> UPPER(sub_76.id)), 0)) FROM tbl_01 AS sub_76 LIMIT 1) AS c_79,
    t_ref_66.id AS c_80
FROM tbl_07 AS t_ref_66
WHERE (SELECT sub_81.col_1 FROM tbl_06 AS sub_81 LIMIT 1)
LIMIT 12;

DELETE FROM customer_profiles WHERE id = 76496;
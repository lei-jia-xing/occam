-- Chaos Fuzz Generator Output (Occam Stress Benchmark)

-- Dialect: POSTGRES

-- Statements: 100

-- AST Expression Depth: 15

-- Injected Defect Case: pg_04

-- Seed: 10012



CREATE TABLE tbl_01 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 JSONB
);

CREATE TABLE tbl_02 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_03 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 BOOLEAN,
    col_3 VARCHAR(64),
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

CREATE TABLE tbl_04 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 BOOLEAN
);

INSERT INTO tbl_04 (id, col_1, col_2) VALUES (745164, -2147483648, FALSE);

CREATE TABLE timeseries_metrics (
    metric_id INT NOT NULL,
    sensor_type VARCHAR(16) NOT NULL,
    recorded_at DATE NOT NULL,
    metric_val DOUBLE PRECISION
) PARTITION BY RANGE (sensor_type, recorded_at);

INSERT INTO tbl_02 (id, col_1, col_2, ref_tbl_01_id) VALUES (90502, 'chaos', 9223372036854775807, 0);

INSERT INTO tbl_04 (id, col_1, col_2) VALUES (161815, -2147483648, FALSE);

UPDATE tbl_04 SET col_2 = FALSE WHERE id > 36;

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, ref_tbl_01_id) VALUES (617460, '', -9223372036854775808, 100);
COMMIT;

UPDATE tbl_03 SET ref_tbl_01_id = -9223372036854775808 WHERE id > 29;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2) VALUES (171627, 100, TRUE);
INSERT INTO timeseries_metrics (id, payload) VALUES (843037, '');
COMMIT;

CREATE TABLE tbl_05 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 VARCHAR(64),
    col_4 JSONB
);

CREATE TABLE tbl_06 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2)
);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (289337, 0, TRUE, 'chaos', 2147483647);

UPDATE tbl_05 SET col_2 = TRUE WHERE id > 81;

CREATE TABLE timeseries_metrics_temp_p1 PARTITION OF timeseries_metrics
    FOR VALUES FROM ('temp', '2023-01-01') TO ('temp', '2023-06-01');

DELETE FROM tbl_04 WHERE id = 9000;

BEGIN;
INSERT INTO timeseries_metrics (id, payload) VALUES (160435, '__fuzz_marker__');
INSERT INTO tbl_01 (id, col_1, col_2) VALUES (791024, 'chaos', '[]');
INSERT INTO tbl_03 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (437291, -1, FALSE, '__fuzz_marker__', -1);
COMMIT;

CREATE VIEW v_summary_1 AS SELECT id FROM tbl_02 WHERE id IS NOT NULL;

CREATE TABLE tbl_07 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    col_3 INT
);

CREATE TABLE tbl_08 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 VARCHAR(64),
    col_4 BOOLEAN
);

INSERT INTO tbl_04 (id, col_1, col_2) VALUES (335770, 0, FALSE);

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (727540, '', TRUE, 'escaped''quote', '[]');
INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (255420, '__fuzz_marker__', TRUE, 'escaped''quote', TRUE);
INSERT INTO tbl_04 (id, col_1, col_2) VALUES (810886, 2147483647, FALSE);
COMMIT;

UPDATE tbl_05 SET col_3 = 'ref_val' WHERE id > 26;

SELECT
    0 AS c_2,
    t_ref_1.id AS c_3,
    (CASE WHEN (SELECT FALSE FROM tbl_02 AS sub_4 LIMIT 1) THEN (SELECT (SELECT inn_7.id FROM tbl_07 AS inn_7 LIMIT 1) FROM tbl_07 AS sub_5 LIMIT 1) ELSE (NULLIF((SELECT (SELECT inn_16.payload FROM timeseries_metrics AS inn_16 LIMIT 1) FROM tbl_01 AS sub_8 LIMIT 1), (NOT ((((((SELECT 'ref_val' FROM tbl_06 AS sub_17 LIMIT 1) + (NULL::INT::TEXT)) <> t_ref_1.ref_tbl_01_id)::INT) / NULLIF((NOT ((CASE WHEN 1 THEN t_ref_1.ref_tbl_01_id ELSE ((NOT (LOWER((((UPPER(t_ref_1.ref_tbl_01_id)::NUMERIC(10,2)) * (t_ref_1.col_1::NUMERIC(10,2))) + t_ref_1.col_3))))::BOOLEAN) END))), 0)))))::INT::TEXT) END) AS c_18
FROM tbl_03 AS t_ref_1
WHERE UPPER(3.14159265)
LIMIT 45;

SELECT
    (t_ref_19.col_1 IN (SELECT sub_20.payload FROM timeseries_metrics AS sub_20)) AS c_23,
    (CASE WHEN ((SELECT (SELECT TRUE FROM tbl_02 AS inn_29 LIMIT 1) FROM tbl_06 AS sub_24 LIMIT 1) - LENGTH(100)) THEN (NOT (t_ref_19.col_1)) ELSE (t_ref_19.id / NULLIF((NOT ((((SELECT (SELECT inn_32.col_2 FROM tbl_06 AS inn_32 LIMIT 1) FROM tbl_02 AS sub_30 LIMIT 1) > NULLIF(NULLIF((CASE WHEN (TRUE IN (SELECT sub_33.col_3 FROM tbl_03 AS sub_33)) THEN 'escaped''quote' ELSE t_ref_19.col_1 END), (SELECT (FALSE::TEXT::VARCHAR(32)) FROM tbl_03 AS sub_34 LIMIT 1)), (SELECT (SELECT inn_36.col_2 FROM tbl_01 AS inn_36 LIMIT 1) FROM tbl_07 AS sub_35 LIMIT 1))) = (NOT ((CASE WHEN t_ref_19.id THEN EXISTS (SELECT 1 FROM tbl_02 AS sub_37 WHERE FALSE) ELSE (TRUE IN (SELECT sub_48.ref_tbl_01_id FROM tbl_02 AS sub_48)) END)))))), 0)) END) AS c_49,
    NULLIF((COALESCE(COALESCE(((SELECT 0.0 FROM tbl_06 AS sub_50 LIMIT 1) - ((SELECT TRUE FROM tbl_08 AS sub_51 LIMIT 1) >= (t_ref_19.id IN (SELECT sub_52.col_2 FROM tbl_02 AS sub_52)))), ((((t_ref_19.col_2 IN (SELECT sub_56.col_2 FROM tbl_01 AS sub_56)) OR NULLIF((((((t_ref_19.id BETWEEN 1 AND 100)::BOOLEAN) IS NULL) <> t_ref_19.col_2) + t_ref_19.col_2), TRUE))::TEXT) - '')), t_ref_19.col_2)::TEXT::VARCHAR(32)), 3.14159265) AS c_57
FROM tbl_01 AS t_ref_19 LEFT JOIN tbl_07 AS j_ref_58 ON t_ref_19.id = j_ref_58.id
WHERE ((SELECT (CASE WHEN (sub_59.col_2::INT::TEXT) THEN ((('escaped''quote' IN (SELECT sub_60.id FROM tbl_06 AS sub_60))::INT) * (SELECT (SELECT (SELECT inn_65.id FROM tbl_05 AS inn_65 LIMIT 1) FROM tbl_03 AS sub_62 LIMIT 1) FROM tbl_04 AS sub_61 LIMIT 1)) ELSE ((NULL IN (SELECT sub_66.col_1 FROM tbl_07 AS sub_66)) > (SELECT (CASE WHEN ((((((SELECT sub_68.id FROM tbl_01 AS sub_68 LIMIT 1) AND (sub_67.id - sub_67.col_2)) / NULLIF(('__fuzz_marker__' < sub_67.col_4), 0))::NUMERIC(10,2))::TEXT)::TEXT::VARCHAR(32)) THEN (SELECT (SELECT inn_70.col_3 FROM tbl_07 AS inn_70 LIMIT 1) FROM tbl_08 AS sub_69 LIMIT 1) ELSE (sub_67.col_2 + ('alpha_beta_gamma' < EXISTS (SELECT 1 FROM tbl_05 AS sub_71 WHERE sub_71.col_1))) END) FROM tbl_05 AS sub_67 LIMIT 1)) END) FROM tbl_08 AS sub_59 LIMIT 1) + t_ref_19.col_1)
LIMIT 9;

CREATE TABLE timeseries_metrics_temp_p2 PARTITION OF timeseries_metrics
    FOR VALUES FROM ('temp', '2023-06-01') TO ('temp', '2024-01-01');

INSERT INTO tbl_07 (id, col_1, col_2, col_3) VALUES (456096, -9223372036854775808, -1e-5, 100);

INSERT INTO timeseries_metrics (id, payload) VALUES (230468, '__fuzz_marker__');

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (129465, 'ref_val', TRUE, '', FALSE);

CREATE INDEX idx_tbl_04_id_1 ON tbl_04 (id);

BEGIN;
INSERT INTO tbl_06 (id, col_1, col_2) VALUES (219107, FALSE, 1.0);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (711688, -1, TRUE, 'ref_val', 0);
INSERT INTO tbl_03 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (664562, 0, FALSE, 'alpha_beta_gamma', 1);
COMMIT;

INSERT INTO tbl_04 (id, col_1, col_2) VALUES (233212, 2147483647, TRUE);

UPDATE tbl_05 SET col_3 = 'escaped''quote' WHERE id > 54;

UPDATE tbl_06 SET col_1 = FALSE WHERE id > 84;

SELECT
    EXISTS (SELECT 1 FROM tbl_02 AS sub_74 WHERE NULL) AS c_81
FROM tbl_05 AS t_ref_73
WHERE UPPER(t_ref_73.col_1)
LIMIT 36;

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (610436, 'alpha_beta_gamma', FALSE, 'ref_val', FALSE);

CREATE TABLE timeseries_metrics_default PARTITION OF timeseries_metrics DEFAULT;

INSERT INTO timeseries_metrics (id, payload) VALUES (772359, 'escaped''quote');

INSERT INTO tbl_08 (id, col_1, col_2, col_3, col_4) VALUES (81213, 'escaped''quote', TRUE, 'escaped''quote', FALSE);

INSERT INTO tbl_07 (id, col_1, col_2, col_3) VALUES (149581, 100, 1.0, 0);

INSERT INTO timeseries_metrics (id, payload) VALUES (429578, 'alpha_beta_gamma');

UPDATE tbl_06 SET col_2 = 3.14159265 WHERE id > 55;

CREATE INDEX idx_timeseries_metrics_payload_2 ON timeseries_metrics (payload);

INSERT INTO tbl_01 (id, col_1, col_2) VALUES (918035, 'chaos', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

DELETE FROM tbl_03 WHERE id = 7372;

CREATE TABLE tbl_09 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 NUMERIC(10,2),
    col_3 NUMERIC(10,2)
);

INSERT INTO tbl_03 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (151669, -9223372036854775808, FALSE, 'chaos', 1);

INSERT INTO timeseries_metrics VALUES
    (1, 'temp', '2023-03-15', 21.5),
    (2, 'temp', '2023-08-20', 32.1),
    (3, 'press', '2023-04-10', 1013.25),
    (4, 'humid', '2023-05-01', 55.0);

SELECT
    ABS((SELECT ((((NOT (ABS((EXISTS (SELECT 1 FROM tbl_01 AS sub_84 WHERE sub_84.col_1) + (sub_83.col_3 IS NOT NULL)))))::INT::TEXT) / NULLIF(sub_83.id, 0)) + sub_83.col_1) FROM tbl_07 AS sub_83 LIMIT 1)) AS c_86
FROM tbl_09 AS t_ref_82
WHERE (CASE WHEN (((((t_ref_82.col_3 / NULLIF(EXISTS (SELECT 1 FROM tbl_09 AS sub_87 WHERE sub_87.col_2), 0)) = (SELECT (SELECT inn_96.col_2 FROM tbl_07 AS inn_96 LIMIT 1) FROM tbl_05 AS sub_89 LIMIT 1)) IS NULL) * UPPER((t_ref_82.col_3::TEXT::VARCHAR(32)))) + ((NULLIF(((CASE WHEN EXISTS (SELECT 1 FROM timeseries_metrics AS sub_97 WHERE sub_97.id) THEN (SELECT ABS(UPPER((CASE WHEN UPPER(('alpha_beta_gamma' IN (SELECT sub_99.id FROM tbl_03 AS sub_99))) THEN sub_98.id ELSE (SELECT (SELECT NULL FROM timeseries_metrics AS inn_101 LIMIT 1) FROM tbl_08 AS sub_100 LIMIT 1) END))) FROM tbl_07 AS sub_98 LIMIT 1) ELSE (t_ref_82.col_3 IS NULL) END) <> (t_ref_82.col_3 - (CASE WHEN (CASE WHEN ((NOT ((-2147483648 * ((SELECT (SELECT 0.0 FROM tbl_09 AS inn_103 LIMIT 1) FROM tbl_02 AS sub_102 LIMIT 1) <= NULL)))) BETWEEN 100 AND -9223372036854775808) THEN t_ref_82.id ELSE ((t_ref_82.id = (t_ref_82.col_3 - (t_ref_82.col_3 IN (SELECT sub_104.col_3 FROM tbl_03 AS sub_104))))::TEXT::VARCHAR(32)) END) THEN (NOT ((t_ref_82.id > EXISTS (SELECT 1 FROM tbl_07 AS sub_105 WHERE sub_105.id)))) ELSE t_ref_82.col_3 END))), NULLIF((SELECT EXISTS (SELECT 1 FROM tbl_08 AS sub_107 WHERE sub_107.col_2) FROM tbl_02 AS sub_106 LIMIT 1), ((((NOT (EXISTS (SELECT 1 FROM tbl_01 AS sub_108 WHERE sub_108.col_1)))::BOOLEAN) BETWEEN 100 AND -1) <> NULLIF(t_ref_82.id, (t_ref_82.id IN (SELECT sub_109.payload FROM timeseries_metrics AS sub_109))))))::TEXT) OR t_ref_82.id)) THEN TRUE ELSE (SELECT (SELECT (0.0000000001 IN (SELECT sub_112.col_1 FROM tbl_08 AS sub_112)) FROM tbl_05 AS sub_111 LIMIT 1) FROM tbl_06 AS sub_110 LIMIT 1) END)
LIMIT 27;

BEGIN;
INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (507042, '', TRUE, 'ref_val', '{}');
INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (908706, 0.0, 999999999.99, 999999999.99);
COMMIT;

CREATE VIEW v_summary_2 AS SELECT id FROM tbl_04 WHERE id IS NOT NULL;

BEGIN;
INSERT INTO tbl_04 (id, col_1, col_2) VALUES (818079, 1, FALSE);
INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (637973, -1.5, 0.0000000001, 0.0000000001);
INSERT INTO tbl_06 (id, col_1, col_2) VALUES (257988, TRUE, 1.0);
COMMIT;

INSERT INTO tbl_06 (id, col_1, col_2) VALUES (524285, FALSE, 1e10);

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (74828, -1e-5, 3.14159265, 1e10);

INSERT INTO tbl_04 (id, col_1, col_2) VALUES (196459, 42, TRUE);

CREATE INDEX idx_tbl_02_id_3 ON tbl_02 (id);

CREATE TABLE tbl_10 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN
);

CREATE INDEX idx_tbl_06_col_2_4 ON tbl_06 (col_2);

CREATE INDEX idx_timeseries_metrics_id_5 ON timeseries_metrics (id);

DELETE FROM tbl_04 WHERE id = 42558;

INSERT INTO timeseries_metrics (id, payload) VALUES (576101, '__fuzz_marker__');

ALTER TABLE tbl_06 DROP COLUMN col_1;

INSERT INTO tbl_01 (id, col_1, col_2) VALUES (965044, 'ref_val', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (519796, 'alpha_beta_gamma', TRUE, '', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');

UPDATE tbl_03 SET col_2 = FALSE WHERE id > 54;

BEGIN;
INSERT INTO tbl_02 (id, col_1, col_2, ref_tbl_01_id) VALUES (752890, 'escaped''quote', 100, -2147483648);
INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (702617, '__fuzz_marker__', TRUE, 'ref_val', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');
COMMIT;

INSERT INTO timeseries_metrics (id, payload) VALUES (866645, '');

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (116862, 3.14159265, 999999999.99, 0.0000000001);

ALTER TABLE tbl_04 ADD COLUMN dyn_col_4 VARCHAR(32);

CREATE TABLE tbl_11 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 NUMERIC(10,2)
);

DELETE FROM tbl_03 WHERE id = 73655;

CREATE TABLE tbl_12 (
    id INT PRIMARY KEY,
    col_1 NUMERIC(10,2),
    col_2 JSONB,
    ref_tbl_01_id INT,
    FOREIGN KEY (ref_tbl_01_id) REFERENCES tbl_01(id)
);

BEGIN;
INSERT INTO tbl_12 (id, col_1, col_2, ref_tbl_01_id) VALUES (239042, -1.5, '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}', 9223372036854775807);
COMMIT;

CREATE INDEX idx_tbl_08_col_1_6 ON tbl_08 (col_1);

CREATE TABLE tbl_13 (
    id INT PRIMARY KEY,
    col_1 BOOLEAN,
    col_2 VARCHAR(64)
);

BEGIN;
INSERT INTO tbl_13 (id, col_1, col_2) VALUES (353241, TRUE, 'alpha_beta_gamma');
INSERT INTO tbl_05 (id, col_1, col_2, col_3, col_4) VALUES (586258, 'ref_val', TRUE, 'chaos', '{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}');
COMMIT;

CREATE TABLE timeseries_metrics (
    metric_id INT NOT NULL,
    sensor_type VARCHAR(16) NOT NULL,
    recorded_at DATE NOT NULL,
    metric_val DOUBLE PRECISION
) PARTITION BY RANGE (sensor_type, recorded_at);

SELECT sensor_type, recorded_at, AVG(metric_val)
FROM timeseries_metrics
WHERE (sensor_type = 'temp' AND recorded_at >= '2023-05-01')
   OR (sensor_type = 'press' AND metric_val > 1000.0)
GROUP BY sensor_type, recorded_at;

CREATE INDEX idx_tbl_09_col_3_7 ON tbl_09 (col_3);

INSERT INTO tbl_01 (id, col_1, col_2) VALUES (46123, 'escaped''quote', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}');

CREATE VIEW v_summary_3 AS SELECT id FROM timeseries_metrics WHERE id IS NOT NULL;

INSERT INTO tbl_03 (id, col_1, col_2, col_3, ref_tbl_01_id) VALUES (55019, 100, FALSE, 'alpha_beta_gamma', 42);

CREATE TABLE tbl_14 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 BOOLEAN,
    col_3 JSONB,
    col_4 JSONB,
    ref_tbl_12_id INT,
    FOREIGN KEY (ref_tbl_12_id) REFERENCES tbl_12(id)
);

CREATE VIEW v_summary_4 AS SELECT id FROM tbl_12 WHERE id IS NOT NULL;

DELETE FROM tbl_09 WHERE id = 54855;

INSERT INTO tbl_13 (id, col_1, col_2) VALUES (482986, TRUE, 'escaped''quote');

CREATE INDEX idx_tbl_01_col_2_8 ON tbl_01 (col_2);

INSERT INTO tbl_07 (id, col_1, col_2, col_3) VALUES (506281, -9223372036854775808, 0.0000000001, -2147483648);

BEGIN;
INSERT INTO timeseries_metrics (id, payload) VALUES (256461, '__fuzz_marker__');
INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (370381, 0.0, 0.0000000001, 0.0);
INSERT INTO timeseries_metrics (id, payload) VALUES (28049, 'ref_val');
COMMIT;

BEGIN;
INSERT INTO tbl_14 (id, col_1, col_2, col_3, col_4, ref_tbl_12_id) VALUES (959521, 'alpha_beta_gamma', FALSE, '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', '{"ref_id": 101, "payload": {"nested": {"v": -1}}}', -2147483648);
INSERT INTO tbl_10 (id, col_1, col_2) VALUES (652923, 'alpha_beta_gamma', TRUE);
INSERT INTO tbl_07 (id, col_1, col_2, col_3) VALUES (115979, 2147483647, 0.0000000001, -2147483648);
COMMIT;

INSERT INTO tbl_10 (id, col_1, col_2) VALUES (627826, 'ref_val', TRUE);

INSERT INTO tbl_04 (id, col_1, col_2, dyn_col_4) VALUES (843073, 42, TRUE, 'alpha_beta_gamma');

DELETE FROM tbl_13 WHERE id = 47216;

CREATE TABLE tbl_15 (
    id INT PRIMARY KEY,
    col_1 VARCHAR(64),
    col_2 INT
);

CREATE TABLE tbl_16 (
    id INT PRIMARY KEY,
    col_1 INT,
    col_2 NUMERIC(10,2),
    ref_tbl_09_id INT,
    FOREIGN KEY (ref_tbl_09_id) REFERENCES tbl_09(id)
);

SELECT
    (((NOT (t_ref_113.id)) AND ((t_ref_113.col_2::TEXT::VARCHAR(32)) + LENGTH(((CASE WHEN LENGTH((SELECT (SELECT inn_116.id FROM tbl_14 AS inn_116 LIMIT 1) FROM tbl_10 AS sub_114 LIMIT 1)) THEN LENGTH(((SELECT (TRUE / NULLIF(((sub_117.id IN (SELECT sub_118.col_2 FROM tbl_09 AS sub_118)) OR ((0 IS NOT NULL)::INT::TEXT)), 0)) FROM tbl_01 AS sub_117 LIMIT 1)::NUMERIC(10,2))) ELSE (CASE WHEN (t_ref_113.col_1 IN (SELECT sub_119.id FROM tbl_13 AS sub_119)) THEN (t_ref_113.col_1 IN (SELECT sub_123.id FROM tbl_11 AS sub_123)) ELSE t_ref_113.id END) END) OR ((t_ref_113.col_1 IN (SELECT sub_127.col_2 FROM tbl_06 AS sub_127))::BOOLEAN))))) / NULLIF(((FALSE IN (SELECT sub_128.id FROM tbl_06 AS sub_128)) * t_ref_113.col_2), 0)) AS c_131
FROM tbl_11 AS t_ref_113 LEFT JOIN tbl_06 AS j_ref_132 ON t_ref_113.id = j_ref_132.id
WHERE LOWER((COALESCE((((NULL = (FALSE * (((t_ref_113.col_2 - (SELECT UPPER((CASE WHEN '__fuzz_marker__' THEN 'alpha_beta_gamma' ELSE sub_133.col_1 END)) FROM tbl_09 AS sub_133 LIMIT 1)) / NULLIF(((((t_ref_113.col_1 IN (SELECT sub_134.col_1 FROM tbl_02 AS sub_134)) BETWEEN -1 AND -1) + FALSE) - (SELECT (CASE WHEN COALESCE(sub_135.id, sub_135.col_3) THEN sub_135.col_2 ELSE (sub_135.id * TRUE) END) FROM tbl_09 AS sub_135 LIMIT 1)), 0)) IS NULL))) OR EXISTS (SELECT 1 FROM tbl_08 AS sub_136 WHERE sub_136.col_1)) / NULLIF(((t_ref_113.col_1 - (t_ref_113.col_1 + (t_ref_113.col_2 IS NOT NULL))) IS NULL), 0)), LOWER(((t_ref_113.id IN (SELECT sub_142.id FROM tbl_04 AS sub_142)) * (SELECT ((SELECT (SELECT NULL FROM tbl_11 AS inn_152 LIMIT 1) FROM tbl_03 AS sub_149 LIMIT 1) BETWEEN 42 AND 1) FROM tbl_10 AS sub_148 LIMIT 1))))::NUMERIC(10,2)))
LIMIT 44;

CREATE INDEX idx_tbl_04_id_9 ON tbl_04 (id);

INSERT INTO tbl_09 (id, col_1, col_2, col_3) VALUES (561367, -1.5, -1e-5, 0.0);
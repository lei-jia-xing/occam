-- Golden Scenario 03: Deep AST Expression Trees & Clause Minimization Stress

-- Focus: Recursive boolean/arithmetic expression simplification & intra-statement clause pruning

-- Target: expr_calc_inputs



CREATE TABLE metrics_cache (id INT PRIMARY KEY, k VARCHAR(32), v NUMERIC(10,2));

CREATE TABLE geo_coords (id INT PRIMARY KEY, lat NUMERIC(8,4), lon NUMERIC(8,4));

CREATE TABLE sensor_payloads (id INT PRIMARY KEY, sensor_id INT, payload TEXT);

CREATE INDEX idx_metrics_k ON metrics_cache (k);

CREATE VIEW v_metrics_top AS SELECT id, v FROM metrics_cache WHERE v > 50.0;

INSERT INTO metrics_cache (id, k, v) VALUES (1, 'key_1', 12.5);

INSERT INTO geo_coords (id, lat, lon) VALUES (1, 30.1000, 110.1000);

INSERT INTO sensor_payloads (id, sensor_id, payload) VALUES (1, 101, 'raw_packet_1');

INSERT INTO metrics_cache (id, k, v) VALUES (2, 'key_2', 25.0);

INSERT INTO geo_coords (id, lat, lon) VALUES (2, 30.2000, 110.2000);

INSERT INTO sensor_payloads (id, sensor_id, payload) VALUES (2, 102, 'raw_packet_2');

INSERT INTO metrics_cache (id, k, v) VALUES (3, 'key_3', 37.5);

INSERT INTO geo_coords (id, lat, lon) VALUES (3, 30.3000, 110.3000);

INSERT INTO sensor_payloads (id, sensor_id, payload) VALUES (3, 103, 'raw_packet_3');

INSERT INTO metrics_cache (id, k, v) VALUES (4, 'key_4', 50.0);

INSERT INTO geo_coords (id, lat, lon) VALUES (4, 30.4000, 110.4000);

INSERT INTO sensor_payloads (id, sensor_id, payload) VALUES (4, 104, 'raw_packet_4');

INSERT INTO metrics_cache (id, k, v) VALUES (5, 'key_5', 62.5);

INSERT INTO geo_coords (id, lat, lon) VALUES (5, 30.5000, 110.5000);

INSERT INTO sensor_payloads (id, sensor_id, payload) VALUES (5, 105, 'raw_packet_5');

INSERT INTO metrics_cache (id, k, v) VALUES (6, 'key_6', 75.0);

INSERT INTO geo_coords (id, lat, lon) VALUES (6, 30.6000, 110.6000);

INSERT INTO sensor_payloads (id, sensor_id, payload) VALUES (6, 106, 'raw_packet_6');

INSERT INTO metrics_cache (id, k, v) VALUES (7, 'key_7', 87.5);

INSERT INTO geo_coords (id, lat, lon) VALUES (7, 30.7000, 110.7000);

INSERT INTO sensor_payloads (id, sensor_id, payload) VALUES (7, 107, 'raw_packet_7');

UPDATE metrics_cache SET v = v * 1.05 WHERE id = 1;

DELETE FROM sensor_payloads WHERE id = 1;

UPDATE metrics_cache SET v = v * 1.05 WHERE id = 2;

DELETE FROM sensor_payloads WHERE id = 2;

UPDATE metrics_cache SET v = v * 1.05 WHERE id = 3;

DELETE FROM sensor_payloads WHERE id = 3;

UPDATE metrics_cache SET v = v * 1.05 WHERE id = 4;

DELETE FROM sensor_payloads WHERE id = 4;

CREATE TABLE expr_calc_inputs (id INT PRIMARY KEY, base_val NUMERIC(12,4), factor_a INT, factor_b INT, flag_active BOOLEAN);

INSERT INTO expr_calc_inputs (id, base_val, factor_a, factor_b, flag_active) VALUES (1, 100.5000, 10, 5, TRUE), (2, 250.7500, 20, 0, FALSE);

SELECT
    id,
    CASE
        WHEN flag_active = TRUE THEN
            COALESCE(
                NULLIF((base_val * factor_a) / NULLIF(factor_b, 0), 0.0),
                CASE
                    WHEN factor_a > 15 THEN (base_val + 100.0)
                    ELSE (base_val - 50.0)
                END
            )
        ELSE
            (base_val * 0.5)
    END AS complex_calc
FROM expr_calc_inputs
WHERE (id > 0 AND (factor_a IS NOT NULL OR factor_b > -1))
  AND (base_val > 50.0 OR flag_active = TRUE)
GROUP BY id, base_val, factor_a, factor_b, flag_active
HAVING COUNT(*) >= 1
ORDER BY id ASC
LIMIT 10;

-- Case ID: pg_12
-- Target DBMS: PostgreSQL (>= 13.0)
-- Bug Class: Assertion Failure (bms_is_subset(lateral_relids, relids))
-- Defect Module: backend/optimizer/plan/subselect.c
-- Reference: PG-BUG-17255 / SQLancer-PG-2021-11
-- Description: Corrupt join relids in lateral reference when flattening nested subqueries
--              with multiple left outer joins.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE device_telemetry (
    reading_id INT PRIMARY KEY,
    device_sn VARCHAR(32),
    reading_val DOUBLE PRECISION
);

CREATE TABLE alert_rules (
    rule_id INT PRIMARY KEY,
    severity INT,
    trigger_condition VARCHAR(128)
);

INSERT INTO device_telemetry (reading_id, device_sn, reading_val) VALUES
    (1, 'SN-001', 98.6),
    (2, 'SN-002', 101.4);

INSERT INTO alert_rules (rule_id, severity, trigger_condition) VALUES
    (10, 1, 'reading_val > 100.0');

-- Target Reproduction Scenario: Multi-table nested lateral join relid set mismatch
CREATE TABLE alpha_tbl (
    a_id INT PRIMARY KEY,
    val_a INT
);

CREATE TABLE beta_tbl (
    b_id INT PRIMARY KEY,
    a_ref INT REFERENCES alpha_tbl(a_id),
    val_b INT
);

CREATE TABLE gamma_tbl (
    c_id INT PRIMARY KEY,
    b_ref INT REFERENCES beta_tbl(b_id),
    val_c INT
);

INSERT INTO alpha_tbl (a_id, val_a) VALUES (1, 10), (2, 20);
INSERT INTO beta_tbl (b_id, a_ref, val_b) VALUES (1, 1, 100), (2, 2, 200);
INSERT INTO gamma_tbl (c_id, b_ref, val_c) VALUES (1, 1, 1000), (2, 2, 2000);

-- Crashing Query: Deep nested correlated lateral subqueries with outer joins
SELECT *
FROM alpha_tbl a
LEFT JOIN (
    beta_tbl b
    LEFT JOIN LATERAL (
        SELECT g.c_id, (g.val_c + a.val_a + b.val_b) AS computed_sum
        FROM gamma_tbl g
        WHERE g.b_ref = b.b_id
          AND g.val_c > a.val_a
    ) lat ON TRUE
) ON a.a_id = b.a_ref
WHERE a.val_a > 5;

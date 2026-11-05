-- Case ID: mysql_16
-- Target DBMS: MySQL (>= 8.0.22)
-- Bug Class: Assertion Failure (item->type() == Item::FIELD_ITEM)
-- Defect Module: sql/window_frame.cc
-- Reference: MySQL-BUG-102877
-- Description: Assertion failure in window frame cursor when evaluating RANGE BETWEEN
--              temporal intervals on nullable DATETIME columns.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE daily_aggregates (
    agg_id INT PRIMARY KEY,
    day_date DATE,
    total_events BIGINT
);

CREATE TABLE system_health (
    probe_id INT PRIMARY KEY,
    probe_name VARCHAR(32),
    is_healthy TINYINT(1)
);

INSERT INTO daily_aggregates (agg_id, day_date, total_events) VALUES
    (1, '2023-11-01', 500000),
    (2, '2023-11-02', 620000);

INSERT INTO system_health (probe_id, probe_name, is_healthy) VALUES
    (10, 'db_read_replica', 1),
    (20, 'cache_cluster', 1);

-- Target Reproduction Scenario: Table with nullable datetime and temporal window range
CREATE TABLE device_heartbeats (
    heartbeat_id INT PRIMARY KEY,
    device_id INT NOT NULL,
    ping_time DATETIME NULL,
    latency_ms INT NOT NULL
);

INSERT INTO device_heartbeats (heartbeat_id, device_id, ping_time, latency_ms) VALUES
    (1, 101, '2023-11-01 10:00:00', 15),
    (2, 101, '2023-11-01 10:00:30', 18),
    (3, 101, NULL, 999),
    (4, 101, '2023-11-01 10:01:00', 14),
    (5, 102, '2023-11-01 10:00:00', 25),
    (6, 102, NULL, 500);

-- Crashing Query: Window framing with temporal interval range over column with NULL values
SELECT
    heartbeat_id,
    device_id,
    ping_time,
    latency_ms,
    AVG(latency_ms) OVER (
        PARTITION BY device_id
        ORDER BY ping_time
        RANGE BETWEEN INTERVAL 1 MINUTE PRECEDING AND INTERVAL 1 MINUTE FOLLOWING
    ) AS rolling_avg_latency
FROM device_heartbeats
ORDER BY device_id, heartbeat_id;

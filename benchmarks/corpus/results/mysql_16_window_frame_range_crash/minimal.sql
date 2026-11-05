-- Case ID: mysql_16
-- Target DBMS: MySQL (>= 8.0.22)
-- Bug Class: Assertion Failure (item->type() == Item::FIELD_ITEM)
-- Defect Module: sql/window_frame.cc
-- Reference: MySQL-BUG-102877
-- Description: Assertion failure in window frame cursor when evaluating RANGE BETWEEN
--              temporal intervals on nullable DATETIME columns.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Table with nullable datetime and temporal window range
CREATE TABLE device_heartbeats (
    heartbeat_id INT PRIMARY KEY,
    device_id INT NOT NULL,
    ping_time DATETIME NULL,
    latency_ms INT NOT NULL
);


-- Crashing Query: Window framing with temporal interval range over column with NULL values
SELECT
    heartbeat_id,
    device_id,
    ping_time,
    latency_ms,
    AVG(latency_ms) OVER (
        PARTITION BY device_id
       
        RANGE BETWEEN INTERVAL 1 MINUTE PRECEDING AND INTERVAL 1 MINUTE FOLLOWING
    ) AS rolling_avg_latency
FROM device_heartbeats
;

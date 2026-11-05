-- Case ID: pg_04
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: Assertion Failure (bms_is_member in partition pruning)
-- Defect Module: backend/partitioning/partprune.c
-- Reference: PG-BUG-17101
-- Description: Run-time partition pruning assertion failure on composite range partition
--              key with DEFAULT partition when evaluating parameterized OR clauses.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Range partitioned table with DEFAULT partition
CREATE TABLE timeseries_metrics (
    metric_id INT NOT NULL,
    sensor_type VARCHAR(16) NOT NULL,
    recorded_at DATE NOT NULL,
    metric_val DOUBLE PRECISION
) PARTITION BY RANGE (sensor_type, recorded_at);

CREATE TABLE timeseries_metrics_temp_p1 PARTITION OF timeseries_metrics
    FOR VALUES FROM ('temp', '2023-01-01') TO ('temp', '2023-06-01');

CREATE TABLE timeseries_metrics_temp_p2 PARTITION OF timeseries_metrics
    FOR VALUES FROM ('temp', '2023-06-01') TO ('temp', '2024-01-01');

CREATE TABLE timeseries_metrics_default PARTITION OF timeseries_metrics DEFAULT;

INSERT INTO timeseries_metrics VALUES
    (1, 'temp', '2023-03-15', 21.5),
    (2, 'temp', '2023-08-20', 32.1),
    (3, 'press', '2023-04-10', 1013.25),
    (4, 'humid', '2023-05-01', 55.0);

-- Crashing Query: Run-time pruning with parameterized disjunctive predicates triggering assertion
SELECT sensor_type, recorded_at, AVG(metric_val)
FROM timeseries_metrics
WHERE (sensor_type = 'temp' AND recorded_at >= '2023-05-01')
   OR (sensor_type = 'press' AND metric_val > 1000.0)
GROUP BY sensor_type, recorded_at;

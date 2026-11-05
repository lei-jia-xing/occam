-- Case ID: pg_07
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: SIGABRT / Assertion Failure
-- Defect Module: backend/access/brin/brin_inclusion.c
-- Reference: PG-BUG-16611
-- Description: BRIN inclusion operator class crash during index summarization of
--              empty and NULL ranges.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE raw_telemetry_dump (
    dump_id BIGINT PRIMARY KEY,
    ingest_node VARCHAR(32),
    raw_bytes BYTEA
);

CREATE TABLE packet_trace (
    trace_id INT PRIMARY KEY,
    protocol VARCHAR(16),
    header_checksum INT
);

INSERT INTO raw_telemetry_dump (dump_id, ingest_node, raw_bytes) VALUES
    (1001, 'node-ingress-0', '\x01020304'),
    (1002, 'node-ingress-1', '\xdeadbeef');

INSERT INTO packet_trace (trace_id, protocol, header_checksum) VALUES
    (1, 'TCP', 44521),
    (2, 'UDP', 12894);

-- Target Reproduction Scenario: BRIN index on range inclusion operator class
CREATE TABLE sensor_ranges (
    record_id INT PRIMARY KEY,
    station_id INT NOT NULL,
    operating_interval INT4RANGE
);

CREATE INDEX idx_sensor_ranges_brin ON sensor_ranges
    USING brin (operating_interval range_inclusion_ops)
    WITH (pages_per_range = 1);

INSERT INTO sensor_ranges (record_id, station_id, operating_interval) VALUES
    (1, 101, '[10, 20)'::int4range),
    (2, 101, 'empty'::int4range),
    (3, 102, NULL),
    (4, 102, '[50, 100)'::int4range),
    (5, 103, 'empty'::int4range),
    (6, 103, '[200, 300)'::int4range);

-- Trigger index summarization across empty and null range pages
SELECT brin_summarize_new_values('idx_sensor_ranges_brin');

-- Crashing Query: Overlap operator query on BRIN range inclusion index
SELECT record_id, station_id, operating_interval
FROM sensor_ranges
WHERE operating_interval && '[15, 25)'::int4range;

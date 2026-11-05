-- Case ID: pg_02
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: SIGSEGV / Assertion Failure
-- Defect Module: backend/access/gin/ginfast.c
-- Reference: PG-BUG-15424
-- Description: GIN fastupdate pending list corruption during concurrent inserts with
--              fastupdate enabled, leading to segfault during ginInsertCleanup.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE cluster_nodes (
    node_id INT PRIMARY KEY,
    hostname VARCHAR(128) NOT NULL,
    ip_addr VARCHAR(45) NOT NULL,
    rack_zone VARCHAR(32)
);

CREATE TABLE job_dispatch_queue (
    job_id BIGINT PRIMARY KEY,
    target_node INT,
    status VARCHAR(20) DEFAULT 'queued',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO cluster_nodes (node_id, hostname, ip_addr, rack_zone) VALUES
    (1, 'node-us-east-01', '10.0.1.11', 'us-east-1a'),
    (2, 'node-us-east-02', '10.0.1.12', 'us-east-1b');

INSERT INTO job_dispatch_queue (job_id, target_node, status) VALUES
    (9001, 1, 'in_progress'),
    (9002, 2, 'completed');

-- Target Reproduction Scenario
CREATE TABLE log_batches (
    batch_id INT PRIMARY KEY,
    source_service VARCHAR(64) NOT NULL,
    tag_ids INT[] NOT NULL
);

CREATE INDEX idx_log_batches_tags_gin ON log_batches USING gin (tag_ids) WITH (fastupdate = on);

INSERT INTO log_batches (batch_id, source_service, tag_ids) VALUES
    (1, 'ingress-proxy', ARRAY[101, 102, 103, 104]),
    (2, 'ingress-proxy', ARRAY[102, 105, 106]),
    (3, 'auth-gateway', ARRAY[201, 202, 101]),
    (4, 'database-pool', ARRAY[301, 302, 303, 304, 305]);

INSERT INTO log_batches (batch_id, source_service, tag_ids)
SELECT g, 'load-generator', ARRAY[g % 50, (g * 3) % 50, (g * 7) % 50]
FROM generate_series(5, 50) g;

-- Flush pending list and trigger index corruption scan
SELECT gin_clean_pending_list('idx_log_batches_tags_gin');

-- Crashing Query: Array containment query on corrupted fastupdate GIN index
SELECT batch_id, source_service
FROM log_batches
WHERE tag_ids @> ARRAY[101, 102]
ORDER BY batch_id;

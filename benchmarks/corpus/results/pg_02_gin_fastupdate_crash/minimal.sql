-- Case ID: pg_02
-- Target DBMS: PostgreSQL (>= 11.0)
-- Bug Class: SIGSEGV / Assertion Failure
-- Defect Module: backend/access/gin/ginfast.c
-- Reference: PG-BUG-15424
-- Description: GIN fastupdate pending list corruption during concurrent inserts with
--              fastupdate enabled, leading to segfault during ginInsertCleanup.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




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
SELECT g, 'load-generator', ARRAY[g, g, g]
FROM generate_series(5, 50) g;

-- Flush pending list and trigger index corruption scan
SELECT gin_clean_pending_list('idx_log_batches_tags_gin');

-- Crashing Query: Array containment query on corrupted fastupdate GIN index
SELECT batch_id, source_service
FROM log_batches
WHERE tag_ids @> ARRAY[101, 102]
;

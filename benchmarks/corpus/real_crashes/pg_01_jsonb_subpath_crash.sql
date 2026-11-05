-- Case ID: pg_01
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: SIGSEGV (Null pointer dereference)
-- Defect Module: backend/utils/adt/jsonpath_exec.c
-- Reference: PG-BUG-16982 / SQLancer-PG-2020-09
-- Description: Nested JSONB path traversal null pointer dereference during
--              jsonb_path_query evaluation with filter predicate and jsonb_strip_nulls.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE sys_service_config (
    config_id INT PRIMARY KEY,
    module_name VARCHAR(64) NOT NULL,
    config_payload TEXT,
    is_active BOOLEAN DEFAULT TRUE
);

CREATE TABLE sys_audit_events (
    event_id INT PRIMARY KEY,
    actor VARCHAR(64),
    action VARCHAR(32),
    timestamp_epoch BIGINT
);

INSERT INTO sys_service_config (config_id, module_name, config_payload, is_active) VALUES
    (1, 'auth', '{"token_ttl": 3600, "allow_anonymous": false}', TRUE),
    (2, 'billing', '{"currency": "USD", "retry_limit": 3}', TRUE),
    (3, 'notifications', '{"webhook_url": "https://hooks.internal/v1"}', FALSE);

INSERT INTO sys_audit_events (event_id, actor, action, timestamp_epoch) VALUES
    (101, 'admin', 'CLUSTER_BOOT', 1600000000),
    (102, 'system', 'METRIC_ROLLUP', 1600000060),
    (103, 'operator', 'TOKEN_REVOKE', 1600000120);

-- Target Reproduction Scenario
CREATE TABLE customer_profiles (
    profile_id INT PRIMARY KEY,
    tenant_id INT NOT NULL,
    account_status VARCHAR(24),
    profile_doc JSONB NOT NULL
);

CREATE INDEX idx_customer_profiles_doc_gin ON customer_profiles USING gin (profile_doc);

INSERT INTO customer_profiles (profile_id, tenant_id, account_status, profile_doc) VALUES
    (1, 10, 'active', '{"user": {"name": "Alice", "contact": {"emails": [{"type": "work", "address": "alice@corp.com"}, {"type": "home", "address": null}]}}, "settings": {"dark_mode": true}}'),
    (2, 10, 'suspended', '{"user": {"name": "Bob", "contact": {"emails": []}}, "metadata": null}'),
    (3, 20, 'active', '{"user": {"name": "Charlie", "contact": null}, "tags": ["beta", "internal"]}'),
    (4, 20, 'pending', '{"user": null, "notes": "unverified registration"}');

UPDATE customer_profiles
SET profile_doc = jsonb_set(profile_doc, '{settings,backup_email}', '"backup@corp.com"')
WHERE profile_id = 1;

-- Crashing Query: Deep jsonpath filter navigation traversing null objects
SELECT
    p.profile_id,
    p.tenant_id,
    jsonb_strip_nulls(p.profile_doc) AS cleaned_doc,
    jsonb_path_query(p.profile_doc, 'lax $.user.contact.emails[*] ? (@.type == "work").address') AS work_email
FROM customer_profiles p
WHERE p.account_status = 'active'
  AND p.profile_doc @> '{"user": {}}'
ORDER BY p.profile_id;

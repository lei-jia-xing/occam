-- Case ID: mysql_03
-- Target DBMS: MySQL (>= 8.0.22)
-- Bug Class: SIGSEGV (Heap out-of-bounds read in Json_path_cache)
-- Defect Module: sql/item_json_func.cc
-- Reference: MySQL-BUG-101180
-- Description: JSON_EXTRACT with multiple wildcard selectors ($.*[*]) on deeply nested
--              heterogeneous JSON documents resulting in out-of-bounds memory access.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE client_sessions (
    session_id VARCHAR(64) PRIMARY KEY,
    ip_address VARCHAR(45),
    expires_at DATETIME
);

CREATE TABLE auth_providers (
    provider_id INT PRIMARY KEY,
    provider_name VARCHAR(32),
    is_enabled TINYINT(1)
);

INSERT INTO client_sessions (session_id, ip_address, expires_at) VALUES
    ('sess_abc123', '192.168.1.10', '2025-01-01 12:00:00'),
    ('sess_xyz789', '192.168.1.20', '2025-01-01 13:00:00');

INSERT INTO auth_providers (provider_id, provider_name, is_enabled) VALUES
    (1, 'okta', 1),
    (2, 'github', 0);

-- Target Reproduction Scenario: Table with complex JSON structure and multi-path extraction
CREATE TABLE doc_registry (
    doc_id INT PRIMARY KEY,
    tenant_id INT NOT NULL,
    payload JSON NOT NULL
);

INSERT INTO doc_registry (doc_id, tenant_id, payload) VALUES
    (1, 100, '{"departments": [{"name": "eng", "roles": [{"title": "dev", "clearance": 3}, {"title": "qa", "clearance": 2}]}, {"name": "hr", "roles": [{"title": "recruiter", "clearance": 1}]}]}'),
    (2, 100, '{"departments": [{"name": "finance", "roles": []}, {"name": "legal", "roles": null}]}'),
    (3, 200, '{"departments": "not_an_array", "extra": [1, 2, [3, 4]]}'),
    (4, 200, '{"departments": [{"roles": [{"title": "exec"}]}]}');

-- Crashing Query: Deep wildcard extraction combined with JSON_LENGTH and JSON_CONTAINS_PATH
SELECT
    doc_id,
    JSON_EXTRACT(payload, '$.departments[*].roles[*].title') AS extracted_titles,
    JSON_UNQUOTE(JSON_EXTRACT(payload, '$.departments[0].name')) AS primary_dept
FROM doc_registry
WHERE JSON_CONTAINS_PATH(payload, 'one', '$.departments[*].roles')
  AND JSON_LENGTH(JSON_EXTRACT(payload, '$.departments[*].roles[*]')) > 0
ORDER BY doc_id;

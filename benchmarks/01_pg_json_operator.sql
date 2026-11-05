-- Benchmark 01: PostgreSQL JSON Path Operators & GIN Index Crash Scenario
-- Target DBMS: PostgreSQL (>= 12.0)
-- Defect Type: GIN index scan corruption / JSONB path extractor crash
-- Description: Complex JSONB documents indexed via GIN, queried using ->>, #>>, and @> operators.
--              Includes 4 irrelevant setup tables to benchmark dead-table slicing.

-- ============================================================================
-- Irrelevant Setup Tables (Candidates for Backward Dependency Elimination)
-- ============================================================================

CREATE TABLE config_parameters (
    param_id INT PRIMARY KEY,
    param_key VARCHAR(128) NOT NULL UNIQUE,
    param_val TEXT NOT NULL,
    is_sensitive BOOLEAN DEFAULT FALSE,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE audit_trail (
    audit_id INT PRIMARY KEY,
    actor_id INT NOT NULL,
    action VARCHAR(64) NOT NULL,
    resource_type VARCHAR(64) NOT NULL,
    performed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE user_accounts (
    user_id INT PRIMARY KEY,
    username VARCHAR(64) NOT NULL UNIQUE,
    email VARCHAR(128) NOT NULL,
    role VARCHAR(32) DEFAULT 'viewer',
    is_active BOOLEAN DEFAULT TRUE
);

CREATE TABLE system_telemetry (
    metric_id INT PRIMARY KEY,
    host_name VARCHAR(64) NOT NULL,
    metric_name VARCHAR(64) NOT NULL,
    metric_value DOUBLE PRECISION NOT NULL,
    recorded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Interleaved Inserts for Irrelevant Tables
INSERT INTO config_parameters (param_id, param_key, param_val, is_sensitive) VALUES
    (1, 'network.timeout_ms', '5000', FALSE),
    (2, 'auth.jwt_secret', 's3cr3t_t0k3n_k3y', TRUE),
    (3, 'cache.max_memory_mb', '2048', FALSE);

INSERT INTO audit_trail (audit_id, actor_id, action, resource_type) VALUES
    (101, 1, 'USER_LOGIN', 'session'),
    (102, 1, 'CONFIG_UPDATE', 'config_parameters'),
    (103, 2, 'EXPORT_METRICS', 'telemetry');

INSERT INTO user_accounts (user_id, username, email, role, is_active) VALUES
    (1, 'admin', 'admin@cluster.internal', 'superadmin', TRUE),
    (2, 'operator_j', 'operator_j@cluster.internal', 'operator', TRUE),
    (3, 'auditor_k', 'auditor_k@cluster.internal', 'auditor', FALSE);

INSERT INTO system_telemetry (metric_id, host_name, metric_name, metric_value) VALUES
    (501, 'srv-node-01', 'cpu_utilization_pct', 74.2),
    (502, 'srv-node-01', 'mem_utilization_pct', 62.8),
    (503, 'srv-node-02', 'disk_io_iops', 1240.0);

-- ============================================================================
-- Target Reproduction Scenario: Complex JSONB Table & GIN Index
-- ============================================================================

CREATE TABLE event_records (
    id INT PRIMARY KEY,
    tenant_id INT NOT NULL,
    source VARCHAR(64) NOT NULL,
    metadata JSONB NOT NULL,
    payload JSONB NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_event_records_payload_gin ON event_records USING gin (payload);
CREATE INDEX idx_event_records_metadata_gin ON event_records USING gin (metadata);

INSERT INTO event_records (id, tenant_id, source, metadata, payload) VALUES
    (1001, 42, 'billing_service',
     '{"environment": "production", "region": "us-east-1", "collector": {"version": "2.4.1", "host": "worker-10"}}',
     '{"event_type": "charge_attempt", "status": "success", "amount_cents": 4999, "customer": {"id": "cust_9921", "tier": "gold"}, "tags": ["billing", "stripe", "auto_renew"]}'),
    (1002, 42, 'auth_service',
     '{"environment": "staging", "region": "us-west-2", "collector": {"version": "2.3.0", "host": "worker-04"}}',
     '{"event_type": "token_refresh", "status": "success", "user_id": "usr_0012", "auth_method": "saml"}'),
    (1003, 42, 'payment_gateway',
     '{"environment": "production", "region": "eu-central-1", "collector": {"version": "2.4.1", "host": "worker-18"}}',
     '{"event_type": "settlement", "status": "failed", "amount_cents": 125000, "customer": {"id": "cust_4402", "tier": "platinum"}, "error": {"code": "ERR_GATEWAY_TIMEOUT", "details": {"reason": "upstream_deadline_exceeded", "retryable": true}}, "tags": ["settlement", "sepa", "retry_pending"]}'),
    (1004, 99, 'payment_gateway',
     '{"environment": "production", "region": "ap-southeast-1", "collector": {"version": "2.4.1", "host": "worker-22"}}',
     '{"event_type": "settlement", "status": "failed", "amount_cents": 32000, "customer": {"id": "cust_8819", "tier": "silver"}, "error": {"code": "ERR_ACCOUNT_FROZEN", "details": {"reason": "compliance_hold", "retryable": false}}, "tags": ["settlement", "risk_hold"]}');

-- Crashing Query: GIN index scan combining path extracts ->>, nested path #>>, and containment @>
SELECT
    id,
    metadata ->> 'environment' AS env,
    payload #>> '{error, details, reason}' AS failure_reason,
    payload ->> 'status' AS status_code
FROM event_records
WHERE payload @> '{"status": "failed"}'
  AND payload #>> '{error, details, retryable}' = 'true'
  AND metadata ->> 'environment' = 'production';

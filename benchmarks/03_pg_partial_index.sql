-- Benchmark 03: PostgreSQL Partial Expression Index & Schema Migrations Crash Scenario
-- Target DBMS: PostgreSQL (>= 11.0)
-- Defect Type: Planner / index scan failure on partial expression index after schema migrations
-- Description: Schema evolves with multiple ALTER TABLE migrations, builds partial expression indexes
--              combining functions and bitwise operations with WHERE filter predicates, then executes
--              index-targeted queries that expose predicate inference / index match defects.

-- ============================================================================
-- Initial Setup: Core Tables & Irrelevant Artifacts
-- ============================================================================

CREATE TABLE session_tokens (
    token_id INT PRIMARY KEY,
    subject VARCHAR(64) NOT NULL,
    token_hash VARCHAR(128) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE audit_events (
    event_id INT PRIMARY KEY,
    action_name VARCHAR(64) NOT NULL,
    performed_by VARCHAR(64) NOT NULL,
    event_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE accounts (
    id INT PRIMARY KEY,
    email VARCHAR(128) NOT NULL,
    status VARCHAR(32) NOT NULL,
    flags INT NOT NULL DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Interleaved Setup Data
INSERT INTO session_tokens (token_id, subject, token_hash) VALUES
    (1, 'worker_node_1', 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'),
    (2, 'worker_node_2', 'ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb');

INSERT INTO audit_events (event_id, action_name, performed_by) VALUES
    (101, 'INITIALIZE_AUTH_SUBMODULE', 'root'),
    (102, 'GENERATE_SESSION_KEY', 'worker_node_1');

INSERT INTO accounts (id, email, status, flags) VALUES
    (1, 'SysAdmin@Enterprise.Internal', 'active', 7),
    (2, 'Developer1@Enterprise.Internal', 'active', 3),
    (3, 'GuestUser@Public.Domain', 'pending', 1),
    (4, 'BannedUser@Public.Domain', 'suspended', 0);

-- ============================================================================
-- Schema Migrations: ALTER TABLE Evolutions
-- ============================================================================

ALTER TABLE accounts ADD COLUMN tier VARCHAR(32) DEFAULT 'standard';
ALTER TABLE accounts ADD COLUMN login_epoch BIGINT DEFAULT 0;
ALTER TABLE accounts ADD COLUMN risk_score NUMERIC(5,2) DEFAULT 0.00;

-- Additional Data Migrations
UPDATE accounts SET tier = 'enterprise', login_epoch = 1700000000, risk_score = 12.50 WHERE id = 1;
UPDATE accounts SET tier = 'enterprise', login_epoch = 1702400000, risk_score = 25.00 WHERE id = 2;
UPDATE accounts SET tier = 'standard',   login_epoch = 1695000000, risk_score = 75.50 WHERE id = 3;
UPDATE accounts SET tier = 'quarantine', login_epoch = 1680000000, risk_score = 99.00 WHERE id = 4;

INSERT INTO accounts (id, email, status, flags, tier, login_epoch, risk_score) VALUES
    (5, 'SecOps_Lead@Enterprise.Internal', 'active', 15, 'enterprise', 1702400000, 5.00),
    (6, 'RogueWorker@Enterprise.Internal', 'active', 11, 'enterprise', 1702400000, 88.50);

-- ============================================================================
-- Partial Expression Indexes Definition
-- ============================================================================

-- Partial index on lower(email) and bitwise flags where account is active and enterprise
CREATE INDEX idx_accounts_expr_partial
    ON accounts (lower(email), (flags & 7))
    WHERE status = 'active' AND tier = 'enterprise';

-- Partial index on normalized date epoch expression where risk score exceeds threshold
CREATE INDEX idx_accounts_login_epoch
    ON accounts ((login_epoch / 86400))
    WHERE risk_score > 50.00;

-- ============================================================================
-- Crashing Queries: Triggering Expression Index Scan & Predicate Match Defects
-- ============================================================================

SELECT
    id,
    lower(email) AS normalized_email,
    flags & 7 AS flag_mask,
    tier,
    risk_score
FROM accounts
WHERE status = 'active'
  AND tier = 'enterprise'
  AND lower(email) = 'sysadmin@enterprise.internal'
  AND (flags & 7) = 3;

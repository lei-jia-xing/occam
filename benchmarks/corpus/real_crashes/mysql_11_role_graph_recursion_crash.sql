-- Case ID: mysql_11
-- Target DBMS: MySQL (>= 8.0.19)
-- Bug Class: Stack Overflow / SIGSEGV (traverse_role_graph)
-- Defect Module: sql/auth/sql_authorization.cc
-- Reference: MySQL-BUG-102201
-- Description: Stack overflow / SIGSEGV during authorization privilege resolution with
--              cyclic role grant hierarchies and default role activation.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE user_tenants (
    tenant_id INT PRIMARY KEY,
    tenant_name VARCHAR(64)
);

CREATE TABLE login_tokens (
    token_id INT PRIMARY KEY,
    user_identity VARCHAR(64),
    last_verified TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO user_tenants (tenant_id, tenant_name) VALUES
    (1, 'Tenant-Alpha'),
    (2, 'Tenant-Beta');

INSERT INTO login_tokens (token_id, user_identity) VALUES
    (101, 'usr_analyst_1'),
    (102, 'usr_operator_2');

-- Target Reproduction Scenario: Role definition and cyclic grants
CREATE ROLE role_ops_l1;
CREATE ROLE role_ops_l2;
CREATE ROLE role_ops_l3;

GRANT role_ops_l1 TO role_ops_l2;
GRANT role_ops_l2 TO role_ops_l3;
GRANT role_ops_l3 TO role_ops_l1;

CREATE TABLE secure_audit_vault (
    vault_id INT PRIMARY KEY,
    payload_hash VARCHAR(64) NOT NULL,
    classification_level INT NOT NULL
);

INSERT INTO secure_audit_vault (vault_id, payload_hash, classification_level) VALUES
    (1, 'hash_99a8b7', 3),
    (2, 'hash_44c2d1', 2);

-- Crashing Query: Privilege evaluation traversal under cyclic role definitions
SELECT
    vault_id,
    payload_hash,
    CURRENT_ROLE() AS active_roles
FROM secure_audit_vault
WHERE classification_level >= 2;

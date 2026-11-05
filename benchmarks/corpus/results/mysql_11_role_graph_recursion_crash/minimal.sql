-- Case ID: mysql_11
-- Target DBMS: MySQL (>= 8.0.19)
-- Bug Class: Stack Overflow / SIGSEGV (traverse_role_graph)
-- Defect Module: sql/auth/sql_authorization.cc
-- Reference: MySQL-BUG-102201
-- Description: Stack overflow / SIGSEGV during authorization privilege resolution with
--              cyclic role grant hierarchies and default role activation.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




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
;

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




-- Target Reproduction Scenario: Role definition and cyclic grants




-- Crashing Query: Privilege evaluation traversal under cyclic role definitions

-- Case ID: pg_11
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Type Confusion / Memory Corruption (ExecEvalCoerceViaIO)
-- Defect Module: backend/executor/execExpr.c
-- Reference: PG-BUG-17029
-- Description: Nested domain check constraint failure on composite type attributes leading
--              to executor type confusion when evaluating CoerceViaIO in UPDATE.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE billing_codes (
    code_id INT PRIMARY KEY,
    code_name VARCHAR(32),
    rate NUMERIC(6,2)
);

CREATE TABLE merchant_lookup (
    merchant_id INT PRIMARY KEY,
    legal_name VARCHAR(128)
);

INSERT INTO billing_codes (code_id, code_name, rate) VALUES
    (1, 'STANDARD_FEE', 15.00),
    (2, 'EXPRESS_FEE', 35.00);

INSERT INTO merchant_lookup (merchant_id, legal_name) VALUES
    (50, 'Acme Retail Corp');

-- Target Reproduction Scenario: Domains defined over composite type fields
CREATE DOMAIN us_zip_code AS TEXT
    CHECK (VALUE ~ '^[0-9]{5}(-[0-9]{4})?$');

CREATE TYPE physical_address AS (
    street TEXT,
    city TEXT,
    zip us_zip_code
);

CREATE TABLE location_registry (
    loc_id INT PRIMARY KEY,
    merchant_id INT,
    address physical_address NOT NULL
);

INSERT INTO location_registry (loc_id, merchant_id, address) VALUES
    (1, 50, ROW('100 Main St', 'Springfield', '12345')::physical_address),
    (2, 50, ROW('200 Elm St', 'Metropolis', '67890-1234')::physical_address);

-- Crashing Query / DML: Updating composite attribute with cast domain coercion in subquery
UPDATE location_registry
SET address.zip = '99999'::us_zip_code
WHERE loc_id = (
    SELECT loc_id
    FROM location_registry
    WHERE (address).city = 'Springfield'
    LIMIT 1
);

SELECT loc_id, (address).street, (address).city, (address).zip
FROM location_registry
WHERE (address).zip ~ '^[0-9]{5}$';

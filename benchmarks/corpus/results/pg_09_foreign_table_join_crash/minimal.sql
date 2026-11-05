-- Case ID: pg_09
-- Target DBMS: PostgreSQL (>= 12.0)
-- Bug Class: Plan Tree Corruption / SIGSEGV (reparameterize_path)
-- Defect Module: backend/optimizer/path/joinpath.c
-- Reference: PG-BUG-17488 / SQLancer-PG-2022-01
-- Description: Planner crash during path reparameterization for lateral joins involving
--              partitioned tables and outer join grouping sets.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Partitioned parent/child tables with grouping sets and lateral join
CREATE TABLE regional_sales (
    sale_id INT NOT NULL,
    country_code VARCHAR(4) NOT NULL,
    sale_amount NUMERIC(10,2) NOT NULL,
    discount_rate NUMERIC(4,2)
) PARTITION BY LIST (country_code);

CREATE TABLE regional_sales_us PARTITION OF regional_sales FOR VALUES IN ('US', 'CA');
CREATE TABLE regional_sales_eu PARTITION OF regional_sales FOR VALUES IN ('DE', 'FR', 'UK');

CREATE TABLE currency_rates (
    code VARCHAR(4) PRIMARY KEY,
    exchange_to_usd NUMERIC(8,4)
);

INSERT INTO currency_rates (code, exchange_to_usd) VALUES
    ('US', 1.0000),
    ('CA', 0.7400),
    ('DE', 1.0800),
    ('FR', 1.0800),
    ('UK', 1.2600);

INSERT INTO regional_sales (sale_id, country_code, sale_amount, discount_rate) VALUES
    (1, 'US', 100.00, 0.05),
    (2, 'CA', 200.00, 0.10),
    (3, 'DE', 150.00, 0.00),
    (4, 'FR', 300.00, 0.15);

-- Crashing Query: Lateral join correlated with partitioned table under GROUPING SETS
SELECT
    rs.country_code,
    cr.exchange_to_usd,
    SUM(rs.sale_amount) AS total_gross,
    calc.net_usd
FROM regional_sales rs
LEFT JOIN currency_rates cr ON rs.country_code = cr.code
LEFT JOIN LATERAL (
    SELECT (rs.sale_amount * (1.0 - COALESCE(rs.discount_rate, 0.0)) * COALESCE(cr.exchange_to_usd, 1.0)) AS net_usd
) calc ON TRUE
GROUP BY GROUPING SETS (
    (rs.country_code, cr.exchange_to_usd, calc.net_usd),
    (rs.country_code),
    ()
);

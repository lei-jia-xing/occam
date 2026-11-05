-- Case ID: mysql_15
-- Target DBMS: MySQL (>= 8.0.22)
-- Bug Class: SIGSEGV (row_purge_remove_sec_if_poss_low)
-- Defect Module: storage/innobase/row/row0purge.cc
-- Reference: MySQL-BUG-98841
-- Description: InnoDB background purge thread crash on secondary index defined over
--              virtual generated column during transaction rollback.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Virtual generated column with secondary index in transaction
CREATE TABLE product_catalog (
    product_id INT PRIMARY KEY,
    category_code VARCHAR(16) NOT NULL,
    base_sku VARCHAR(32) NOT NULL,
    virtual_composite_sku VARCHAR(64) GENERATED ALWAYS AS CONCAT(category_code, ':', base_sku) VIRTUAL,
    price DECIMAL(10,2) NOT NULL,
    INDEX idx_virtual_sku (virtual_composite_sku)
) ENGINE=InnoDB;

INSERT INTO product_catalog (product_id, category_code, base_sku, price) VALUES
    (1, 'ELEC', 'KB-100', 49.99),
    (2, 'ELEC', 'MS-200', 29.99),
    (3, 'HOME', 'LAMP-01', 19.99);

-- Transaction inducing purge history and undo record generation
START TRANSACTION;

UPDATE product_catalog
SET base_sku = 'KB-100-PRO'
;


-- Rollback to trigger purge thread race condition on virtual index
ROLLBACK;

-- Crashing Query: Index range scan over virtual column secondary index
SELECT
    product_id,
    virtual_composite_sku,
    price
FROM product_catalog
WHERE virtual_composite_sku LIKE 'ELEC:%'
;

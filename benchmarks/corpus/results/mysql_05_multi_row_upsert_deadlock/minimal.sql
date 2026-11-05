-- Case ID: mysql_05
-- Target DBMS: MySQL (>= 8.0.20)
-- Bug Class: Assertion Failure / SIGABRT (btr_pcur_restore_position)
-- Defect Module: storage/innobase/row/row0ins.cc
-- Reference: MySQL-BUG-98380
-- Description: Multi-row INSERT ... ON DUPLICATE KEY UPDATE with multiple composite
--              unique indexes and auto-increment leading to B-tree cursor desync.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)



-- Target Reproduction Scenario: Table with PK and two conflicting UNIQUE keys
CREATE TABLE warehouse_inventory (
    item_id INT AUTO_INCREMENT PRIMARY KEY,
    sku_code VARCHAR(32) NOT NULL,
    warehouse_id INT NOT NULL,
    bin_location VARCHAR(32) NOT NULL,
    stock_qty INT DEFAULT 0,
    CONSTRAINT uk_sku_wh UNIQUE (sku_code, warehouse_id),
    CONSTRAINT uk_bin UNIQUE (warehouse_id, bin_location)
) ENGINE=InnoDB;

INSERT INTO warehouse_inventory (sku_code, warehouse_id, bin_location, stock_qty) VALUES
    ('SKU-A', 1, 'BIN-101', 50),
    ('SKU-B', 1, 'BIN-102', 30),
    ('SKU-C', 2, 'BIN-201', 100);

-- Crashing DML: Batch multi-row upsert triggering simultaneous collision across both distinct unique keys
INSERT INTO warehouse_inventory (sku_code, warehouse_id, bin_location, stock_qty) VALUES
    ('SKU-A', 1, 'BIN-102', 15),
    ('SKU-B', 1, 'BIN-101', 25)
ON DUPLICATE KEY UPDATE
    stock_qty = warehouse_inventory.stock_qty;


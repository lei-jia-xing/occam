-- Benchmark 04: MySQL Multi-Column Unique Key & ON DUPLICATE KEY UPDATE Crash Scenario
-- Target DBMS: MySQL (>= 8.0)
-- Defect Type: InnoDB lock deadlock / secondary unique index corruption during multi-column upsert
-- Description: Table configured with multiple composite unique indexes (primary key, sku+channel,
--              warehouse+bin). Batch multi-row INSERTs with ON DUPLICATE KEY UPDATE trigger concurrent
--              unique index resolution and lock tree edge cases.

-- ============================================================================
-- Unrelated Noise Tables (Dead DDL / DML for Slicing Verification)
-- ============================================================================

CREATE TABLE audit_logs (
    log_id INT PRIMARY KEY,
    event_type VARCHAR(64) NOT NULL,
    actor VARCHAR(64) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE client_sessions (
    session_id VARCHAR(64) PRIMARY KEY,
    client_ip VARCHAR(45) NOT NULL,
    expires_at TIMESTAMP NOT NULL
);

INSERT INTO audit_logs (log_id, event_type, actor) VALUES
    (1, 'SERVICE_BOOT', 'system'),
    (2, 'INVENTORY_JOB_START', 'scheduler');

INSERT INTO client_sessions (session_id, client_ip, expires_at) VALUES
    ('sess_9001', '10.0.4.12', TIMESTAMP '2026-12-31 23:59:59'),
    ('sess_9002', '10.0.4.15', TIMESTAMP '2026-12-31 23:59:59');

-- ============================================================================
-- Target Table: Multiple Multi-Column Unique Constraints
-- ============================================================================

CREATE TABLE inventory_items (
    item_sku VARCHAR(64) NOT NULL,
    warehouse_id INT NOT NULL,
    distribution_channel VARCHAR(32) NOT NULL,
    bin_location VARCHAR(32) NOT NULL,
    stock_count INT NOT NULL DEFAULT 0,
    reserved_count INT NOT NULL DEFAULT 0,
    reorder_point INT NOT NULL DEFAULT 10,
    last_restocked TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (item_sku, warehouse_id),
    UNIQUE KEY uk_sku_channel (item_sku, distribution_channel),
    UNIQUE KEY uk_warehouse_bin (warehouse_id, bin_location)
);

-- ============================================================================
-- Batch Inserts with ON DUPLICATE KEY UPDATE
-- ============================================================================

-- Batch 1: Initial Population across multiple warehouses and channels
INSERT INTO inventory_items (item_sku, warehouse_id, distribution_channel, bin_location, stock_count, reserved_count, reorder_point)
VALUES
    ('SKU-100-A', 1, 'ONLINE_DIRECT', 'BIN-01-A', 50, 5, 10),
    ('SKU-100-B', 1, 'ONLINE_DIRECT', 'BIN-01-B', 120, 15, 20),
    ('SKU-200-A', 2, 'RETAIL_STORE',  'BIN-02-A', 80, 0, 15),
    ('SKU-300-X', 2, 'WHOLESALE_B2B', 'BIN-02-B', 500, 50, 100)
ON DUPLICATE KEY UPDATE
    stock_count = stock_count + VALUES(stock_count),
    reserved_count = reserved_count + VALUES(reserved_count),
    last_restocked = CURRENT_TIMESTAMP;

-- Interleaved Noise DML
UPDATE audit_logs SET actor = 'scheduler_v2' WHERE log_id = 2;

-- Batch 2: Upsert batch conflicting on primary key (item_sku, warehouse_id)
INSERT INTO inventory_items (item_sku, warehouse_id, distribution_channel, bin_location, stock_count, reserved_count, reorder_point)
VALUES
    ('SKU-100-A', 1, 'ONLINE_DIRECT', 'BIN-01-A', 25, 2, 10),
    ('SKU-200-A', 2, 'RETAIL_STORE',  'BIN-02-A', 40, 10, 15),
    ('SKU-400-Z', 1, 'WHOLESALE_B2B', 'BIN-01-C', 300, 20, 50)
ON DUPLICATE KEY UPDATE
    stock_count = inventory_items.stock_count + VALUES(stock_count),
    reserved_count = inventory_items.reserved_count + VALUES(reserved_count),
    reorder_point = GREATEST(inventory_items.reorder_point, VALUES(reorder_point));

-- Batch 3: Upsert triggering secondary composite unique key conflict (uk_sku_channel)
INSERT INTO inventory_items (item_sku, warehouse_id, distribution_channel, bin_location, stock_count, reserved_count, reorder_point)
VALUES
    ('SKU-100-A', 2, 'ONLINE_DIRECT', 'BIN-02-C', 15, 0, 5)
ON DUPLICATE KEY UPDATE
    stock_count = stock_count + VALUES(stock_count),
    reserved_count = reserved_count + VALUES(reserved_count);

-- ============================================================================
-- Crashing Query: Projection and filtering over multi-column keyed dataset
-- ============================================================================

SELECT
    item_sku,
    warehouse_id,
    distribution_channel,
    stock_count,
    reserved_count,
    (stock_count - reserved_count) AS available_units
FROM inventory_items
WHERE warehouse_id IN (1, 2)
  AND (stock_count - reserved_count) < reorder_point
ORDER BY available_units ASC;

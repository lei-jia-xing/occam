-- Seed 05: Upsert Conflict Resolution & Uniqueness Constraints
-- Tests: INSERT ON CONFLICT DO UPDATE, constraint-driven dependencies,
-- and conditional target writes.

CREATE TABLE item_inventory (
    sku VARCHAR(32) PRIMARY KEY,
    item_name VARCHAR(64) NOT NULL,
    quantity INT NOT NULL,
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO item_inventory (sku, item_name, quantity) VALUES
    ('SKU-001', 'Widget Alpha', 50),
    ('SKU-002', 'Gadget Beta', 20);

INSERT INTO item_inventory (sku, item_name, quantity) VALUES
    ('SKU-001', 'Widget Alpha', 30)
ON CONFLICT (sku) DO UPDATE SET
    quantity = item_inventory.quantity + EXCLUDED.quantity;

SELECT sku, item_name, quantity
FROM item_inventory
WHERE quantity >= 50;

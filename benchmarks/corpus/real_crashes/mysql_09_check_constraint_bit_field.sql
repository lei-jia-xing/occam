-- Case ID: mysql_09
-- Target DBMS: MySQL (>= 8.0.19)
-- Bug Class: SIGSEGV (Buffer overrun in Item_func_bit::val_int)
-- Defect Module: sql/item_func.cc
-- Reference: MySQL-BUG-101450
-- Description: CHECK constraint evaluation crash when validating bit fields BIT(N)
--              under multi-row INSERT with bitwise arithmetic expressions.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE user_preferences (
    pref_id INT PRIMARY KEY,
    user_id INT,
    theme VARCHAR(16) DEFAULT 'dark'
);

CREATE TABLE feature_flags (
    flag_id INT PRIMARY KEY,
    flag_key VARCHAR(64) UNIQUE,
    is_active TINYINT(1)
);

INSERT INTO user_preferences (pref_id, user_id, theme) VALUES
    (1, 100, 'light'),
    (2, 200, 'dark');

INSERT INTO feature_flags (flag_id, flag_key, is_active) VALUES
    (1, 'beta_pipeline', 1),
    (2, 'new_indexer', 0);

-- Target Reproduction Scenario: Table with BIT columns and bitwise CHECK constraint
CREATE TABLE network_packet_rules (
    rule_id INT PRIMARY KEY,
    proto_flags BIT(16) NOT NULL,
    mask_filter BIT(16) NOT NULL,
    priority INT NOT NULL,
    CONSTRAINT chk_valid_mask CHECK (BIN(proto_flags & mask_filter) != '0')
);

-- Crashing DML: Multi-row insert evaluating bitwise CHECK constraint on boundary bits
INSERT INTO network_packet_rules (rule_id, proto_flags, mask_filter, priority) VALUES
    (1, b'0000000000000001', b'0000000000000001', 10),
    (2, b'1000000000000000', b'1000000000000000', 20),
    (3, b'1111000011110000', b'0000111100001111', 30);

SELECT
    rule_id,
    BIN(proto_flags) AS bin_flags,
    BIN(mask_filter) AS bin_mask,
    priority
FROM network_packet_rules
WHERE (proto_flags & mask_filter) > b'0';

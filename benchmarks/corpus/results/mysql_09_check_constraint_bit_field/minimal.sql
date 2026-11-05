-- Case ID: mysql_09
-- Target DBMS: MySQL (>= 8.0.19)
-- Bug Class: SIGSEGV (Buffer overrun in Item_func_bit::val_int)
-- Defect Module: sql/item_func.cc
-- Reference: MySQL-BUG-101450
-- Description: CHECK constraint evaluation crash when validating bit fields BIT(N)
--              under multi-row INSERT with bitwise arithmetic expressions.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Table with BIT columns and bitwise CHECK constraint
CREATE TABLE network_packet_rules (
    rule_id INT PRIMARY KEY,
    proto_flags BIT(16) NOT NULL,
    mask_filter BIT(16) NOT NULL,
    priority INT NOT NULL,
    CONSTRAINT chk_valid_mask CHECK (BIN(proto_flags))
);

-- Crashing DML: Multi-row insert evaluating bitwise CHECK constraint on boundary bits
INSERT INTO network_packet_rules (rule_id, proto_flags, mask_filter, priority) VALUES
    (1, b'0000000000000001', b'0000000000000001', 10),
    (2, b'1000000000000000', b'1000000000000000', 20),
    (3, b'1111000011110000', b'0000111100001111', 30);


-- Case ID: mysql_13
-- Target DBMS: MySQL (>= 8.0.22)
-- Bug Class: Assertion Failure / SIGABRT (part_share->auto_inc_initialized)
-- Defect Module: sql/ha_partition.cc
-- Reference: MySQL-BUG-99520
-- Description: Server crash during ALTER TABLE ... EXCHANGE PARTITION with auto-increment
--              counter desynchronization across range partitions.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE staging_records (
    record_id INT PRIMARY KEY,
    source_feed VARCHAR(64),
    staged_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE migration_status (
    mig_id INT PRIMARY KEY,
    phase_name VARCHAR(32),
    is_done TINYINT(1)
);

INSERT INTO staging_records (record_id, source_feed) VALUES
    (1, 'feed_primary'),
    (2, 'feed_secondary');

INSERT INTO migration_status (mig_id, phase_name, is_done) VALUES
    (101, 'schema_sync', 1),
    (102, 'data_cutover', 0);

-- Target Reproduction Scenario: Partitioned table with auto-increment and partition exchange
CREATE TABLE sales_partitioned (
    sale_id INT NOT NULL AUTO_INCREMENT,
    sale_year INT NOT NULL,
    amount DECIMAL(10,2) NOT NULL,
    PRIMARY KEY (sale_id, sale_year)
) ENGINE=InnoDB
PARTITION BY RANGE (sale_year) (
    PARTITION p2022 VALUES LESS THAN (2023),
    PARTITION p2023 VALUES LESS THAN (2024),
    PARTITION pmax VALUES LESS THAN MAXVALUE
);

CREATE TABLE sales_2023 (
    sale_id INT NOT NULL AUTO_INCREMENT,
    sale_year INT NOT NULL,
    amount DECIMAL(10,2) NOT NULL,
    PRIMARY KEY (sale_id, sale_year)
) ENGINE=InnoDB;

INSERT INTO sales_partitioned (sale_year, amount) VALUES
    (2022, 1500.00),
    (2023, 2200.00),
    (2023, 3100.00);

INSERT INTO sales_2023 (sale_year, amount) VALUES
    (2023, 4400.00),
    (2023, 5100.00);

-- Trigger Partition Exchange
ALTER TABLE sales_partitioned EXCHANGE PARTITION p2023 WITH TABLE sales_2023;

-- Crashing DML: Insert into exchanged partition triggering auto_inc assertion
INSERT INTO sales_partitioned (sale_year, amount) VALUES (2023, 9999.00);

SELECT sale_id, sale_year, amount FROM sales_partitioned ORDER BY sale_id;

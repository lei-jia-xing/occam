-- Case ID: mysql_13
-- Target DBMS: MySQL (>= 8.0.22)
-- Bug Class: Assertion Failure / SIGABRT (part_share->auto_inc_initialized)
-- Defect Module: sql/ha_partition.cc
-- Reference: MySQL-BUG-99520
-- Description: Server crash during ALTER TABLE ... EXCHANGE PARTITION with auto-increment
--              counter desynchronization across range partitions.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




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


INSERT INTO sales_partitioned (sale_year, amount) VALUES
    (2022, 1500.00),
    (2023, 2200.00),
    (2023, 3100.00);


-- Trigger Partition Exchange
ALTER TABLE sales_partitioned EXCHANGE PARTITION p2023 WITH TABLE sales_2023;

-- Crashing DML: Insert into exchanged partition triggering auto_inc assertion
INSERT INTO sales_partitioned (sale_year, amount) VALUES (2023, 9999.00);


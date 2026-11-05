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




-- Target Reproduction Scenario: Partitioned table with auto-increment and partition exchange
 



-- Trigger Partition Exchange
 
-- Crashing DML: Insert into exchanged partition triggering auto_inc assertion


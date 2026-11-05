-- Case ID: mysql_15
-- Target DBMS: MySQL (>= 8.0.22)
-- Bug Class: SIGSEGV (row_purge_remove_sec_if_poss_low)
-- Defect Module: storage/innobase/row/row0purge.cc
-- Reference: MySQL-BUG-98841
-- Description: InnoDB background purge thread crash on secondary index defined over
--              virtual generated column during transaction rollback.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE temp_staging (
    stage_id INT PRIMARY KEY,
    batch_num INT,
    status VARCHAR(20) DEFAULT 'unprocessed'
);




-- Target Reproduction Scenario: Virtual generated column with secondary index in transaction


-- Transaction inducing purge history and undo record generation



-- Rollback to trigger purge thread race condition on virtual index

-- Crashing Query: Index range scan over virtual column secondary index

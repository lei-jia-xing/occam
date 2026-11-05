-- Case ID: mysql_14
-- Target DBMS: MySQL (>= 8.0.21)
-- Bug Class: Assertion Failure (thd->lex == lex)
-- Defect Module: sql/sql_prepare.cc
-- Reference: MySQL-BUG-103112
-- Description: Assertion failure when re-preparing statement referencing a view
--              after underlying base table schema alteration.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE query_stats (
    stat_id INT PRIMARY KEY,
    query_digest VARCHAR(64),
    exec_count BIGINT
);




-- Target Reproduction Scenario: Table, View, and Prepared Statement Reparse



-- Prepare statement referencing view


-- Invalidate schema cache by altering base table

-- Crashing Statement: Re-execution forcing re-parse and LEX mismatch assertion

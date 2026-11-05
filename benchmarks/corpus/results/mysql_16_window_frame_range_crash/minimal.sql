-- Case ID: mysql_16
-- Target DBMS: MySQL (>= 8.0.22)
-- Bug Class: Assertion Failure (item->type() == Item::FIELD_ITEM)
-- Defect Module: sql/window_frame.cc
-- Reference: MySQL-BUG-102877
-- Description: Assertion failure in window frame cursor when evaluating RANGE BETWEEN
--              temporal intervals on nullable DATETIME columns.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)
CREATE TABLE daily_aggregates (
    agg_id INT PRIMARY KEY,
    day_date DATE,
    total_events BIGINT
);




-- Target Reproduction Scenario: Table with nullable datetime and temporal window range


-- Crashing Query: Window framing with temporal interval range over column with NULL values

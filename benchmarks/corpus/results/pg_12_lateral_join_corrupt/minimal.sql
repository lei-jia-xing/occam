-- Case ID: pg_12
-- Target DBMS: PostgreSQL (>= 13.0)
-- Bug Class: Assertion Failure (bms_is_subset(lateral_relids, relids))
-- Defect Module: backend/optimizer/plan/subselect.c
-- Reference: PG-BUG-17255 / SQLancer-PG-2021-11
-- Description: Corrupt join relids in lateral reference when flattening nested subqueries
--              with multiple left outer joins.

-- Auxiliary Setup Tables (Candidate for Dead-Table Slicing)




-- Target Reproduction Scenario: Multi-table nested lateral join relid set mismatch
CREATE TABLE alpha_tbl (
    a_id INT PRIMARY KEY,
    val_a INT
);




-- Crashing Query: Deep nested correlated lateral subqueries with outer joins
SELECT *
FROM alpha_tbl a
LEFT JOIN (
    beta_tbl b
    LEFT JOIN LATERAL (
        SELECT g.c_id, g.val_c AS computed_sum
        FROM gamma_tbl g
        WHERE g.b_ref
    ) lat ON TRUE
) ON a.a_id
;

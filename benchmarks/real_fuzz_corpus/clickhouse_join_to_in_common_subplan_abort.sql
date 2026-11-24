CREATE TABLE t (a UInt32, b Nullable(Int64)) ENGINE = MergeTree ORDER BY a;
INSERT INTO t VALUES (1, 1), (2, NULL), (3, 5);

SELECT count() FROM t WHERE 1 > (SELECT max(b))
SETTINGS allow_experimental_correlated_subqueries = 1,
         query_plan_convert_join_to_in = 1;

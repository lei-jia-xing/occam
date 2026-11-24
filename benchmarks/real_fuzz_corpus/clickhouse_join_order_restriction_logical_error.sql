CREATE TABLE t0 (c0 UInt64) ENGINE = MergeTree ORDER BY tuple();
CREATE TABLE t1 (c0 Int64, c1 String) ENGINE = MergeTree ORDER BY tuple();
CREATE TABLE t3 (c1 UInt64, c2 String) ENGINE = MergeTree ORDER BY tuple();

INSERT INTO t0 SELECT number FROM numbers(1000);
INSERT INTO t1 VALUES (1, 'a');
INSERT INTO t3 VALUES (1, 'a');

SELECT *
FROM t1, t3, t0
JOIN            t3 AS right_0 ON (t1.c1 = right_0.c2)
LEFT OUTER JOIN t3 AS right_1 ON (t1.c1 = right_1.c2) AND (right_1.c2 IS NULL)
WHERE t1.c0 < t3.c1;

CREATE TABLE t1 (col_int int, col_double double, j json);
INSERT INTO t1 VALUES (382218415, -36452.389, '{"key1": 220655528}');
SELECT col_int FROM t1
WHERE CASE WHEN POWER(col_double, col_int) THEN j ELSE j END;

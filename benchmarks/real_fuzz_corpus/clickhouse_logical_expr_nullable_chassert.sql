CREATE TABLE t1 (c0 Int) ENGINE = MergeTree() ORDER BY tuple();
INSERT INTO t1 VALUES (1), (2);

CREATE TABLE t1__fuzz_28 (c0 Nullable(Int)) ENGINE = MergeTree() ORDER BY tuple()
    SETTINGS allow_nullable_key = 1;

SET enable_analyzer = 1;

SELECT 1 AS x
FROM t1 AS tx ARRAY JOIN [] AS a0
LEFT JOIN t1__fuzz_28 ON and(t1.c0 = a0, (SELECT t1.c0) > 0, t1.c0 != a0)
FORMAT Null;

DROP TABLE IF EXISTS t05215;
CREATE TABLE t05215 (jd Array(JSON(max_dynamic_types=1))) ENGINE = MergeTree ORDER BY tuple();
INSERT INTO t05215 VALUES
    ([CAST('{"a":1}', 'JSON(max_dynamic_types=1)')]),
    ([CAST('{"a":2}', 'JSON(max_dynamic_types=1)')]),
    ([CAST('{"a":3}', 'JSON(max_dynamic_types=1)')]),
    ([CAST('{"a":[{"k":1}]}', 'JSON(max_dynamic_types=1)')]);
SELECT arrayMap(x -> isDynamicElementInSharedData(x), jd.a) FROM t05215;
SELECT jd.a.:`Array(JSON)`.size0 FROM t05215;

DROP TABLE IF EXISTS mv_loop_scalar;
DROP TABLE IF EXISTS mv_loop_join;
DROP TABLE IF EXISTS dst;
DROP TABLE IF EXISTS src;

CREATE TABLE src (k UInt32) ENGINE = MergeTree ORDER BY k;
CREATE TABLE dst (name String, k UInt32, c UInt64) ENGINE = MergeTree ORDER BY (name, k);

INSERT INTO src VALUES (1);

CREATE MATERIALIZED VIEW mv_loop_scalar TO dst AS
    SELECT 'scalar' AS name, k, (SELECT count() FROM (SELECT k FROM loop(currentDatabase(), src) LIMIT 5)) AS c FROM src;
CREATE MATERIALIZED VIEW mv_loop_join TO dst AS
    SELECT 'join' AS name, s.k AS k, t.c AS c FROM src AS s CROSS JOIN (SELECT count() AS c FROM (SELECT k FROM loop(currentDatabase(), src) LIMIT 5)) AS t;

INSERT INTO src VALUES (2), (3);

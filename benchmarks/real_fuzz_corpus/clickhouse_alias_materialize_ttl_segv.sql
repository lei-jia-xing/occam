SET allow_experimental_alias_table_engine = 1;

CREATE TABLE tgt (i Int32, d Date) ENGINE = MergeTree ORDER BY i TTL d + INTERVAL 1 DAY;
CREATE TABLE av ENGINE = Alias('tgt');

ALTER TABLE av MATERIALIZE TTL;

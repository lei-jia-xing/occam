DROP TABLE IF EXISTS t_alter_key_type;
CREATE TABLE t_alter_key_type (k UInt64, x Nullable(UInt8))
ENGINE = MergeTree ORDER BY (k, CAST(x, 'UInt8'));
INSERT INTO t_alter_key_type VALUES (1, 5);
DETACH TABLE t_alter_key_type SYNC;
ATTACH TABLE t_alter_key_type;
ALTER TABLE t_alter_key_type ADD COLUMN y UInt8 DEFAULT 0 SETTINGS cast_keep_nullable = 1;
INSERT INTO t_alter_key_type (k, x) VALUES (2, 7);

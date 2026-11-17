-- XVAL 04: maintenance statements, characterizing their authorizer
-- footprint: ANALYZE (ANALYZE action, no occam counterpart),
-- REINDEX (occam frontend gap: parse error, engine succeeds),
-- VACUUM (occam fences UnknownStatement; engine replays the schema
-- into a vacuum_* db with ATTACH/TRANSACTION/CREATE/INSERT traffic).
CREATE TABLE m (a INTEGER PRIMARY KEY, b TEXT);
CREATE INDEX mi ON m(b);
INSERT INTO m VALUES (1, 'x'), (2, 'y');
ANALYZE;
REINDEX;
VACUUM;

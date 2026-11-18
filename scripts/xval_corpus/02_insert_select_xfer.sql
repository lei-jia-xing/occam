-- XVAL 02: INSERT..SELECT incl. the transfer (xfer) optimization case.
-- Pins the xfer blind spot in-corpus: an xfer-eligible INSERT reports
-- INSERT with zero source READs (stable 20/20 trials), so occam's
-- (correct, conservative) source read has no engine counterpart.
-- NOTE: same-table INSERT..SELECT does NOT take xfer (engine still
-- reports source READs); the xfer shape is two tables with identical
-- schemas and a plain SELECT * with no WHERE/LIMIT.
CREATE TABLE isel_src (a INTEGER PRIMARY KEY, b TEXT NOT NULL);
CREATE TABLE isel_dst (a INTEGER PRIMARY KEY, b TEXT NOT NULL);
INSERT INTO isel_src VALUES (1, 'x'), (2, 'y');
INSERT INTO isel_dst SELECT * FROM isel_src;
INSERT INTO isel_src VALUES (3, 'z');
INSERT INTO isel_dst SELECT a, b FROM isel_src WHERE a > 2;
INSERT INTO isel_src SELECT a + 100, b FROM isel_src WHERE a < 100;

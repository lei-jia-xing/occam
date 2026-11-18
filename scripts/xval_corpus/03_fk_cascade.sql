-- XVAL 03: FK-heavy script (multi-table, cascades), valid with
-- enforcement both OFF (default) and ON (--fk-on variant).
-- FK-off: no enforcement traffic, all statements comparable.
-- FK-on: enforcement reads + cascade writes surface unsourced and
-- become comparable (R-fk-off lifted, triaged per statement).
CREATE TABLE dept (
    did INTEGER PRIMARY KEY,
    dname TEXT NOT NULL
);
CREATE TABLE emp (
    eid INTEGER PRIMARY KEY,
    did INTEGER NOT NULL REFERENCES dept(did) ON DELETE CASCADE ON UPDATE CASCADE,
    ename TEXT NOT NULL
);
CREATE TABLE task (
    tid INTEGER PRIMARY KEY,
    eid INTEGER NOT NULL REFERENCES emp(eid) ON DELETE CASCADE,
    title TEXT NOT NULL
);
INSERT INTO dept VALUES (1, 'eng'), (2, 'ops');
INSERT INTO emp VALUES (10, 1, 'amy'), (20, 2, 'bob');
INSERT INTO task VALUES (100, 10, 'build'), (200, 20, 'deploy');
UPDATE dept SET did = 7 WHERE did = 2;
DELETE FROM dept WHERE did = 1;

-- XVAL 06: mid-file PRAGMA foreign_keys=ON toggle (default mode).
-- Enforcement starts OFF; the toggle threads forward, so the parent DELETE
-- after it expands to a cascade child write on both sides (Enforcement.hs
-- session threading mirrors the engine replay exactly).
CREATE TABLE tgl_p (
    pid INTEGER PRIMARY KEY,
    pname TEXT NOT NULL
);
CREATE TABLE tgl_c (
    cid INTEGER PRIMARY KEY,
    pid INTEGER NOT NULL REFERENCES tgl_p(pid) ON DELETE CASCADE,
    cname TEXT NOT NULL
);
INSERT INTO tgl_p VALUES (1, 'one'), (2, 'two');
INSERT INTO tgl_c VALUES (10, 1, 'a'), (20, 2, 'b');
PRAGMA foreign_keys=ON;
DELETE FROM tgl_p WHERE pid = 1;
SELECT cid, pid FROM tgl_c WHERE pid = 2;
SELECT cid, pid FROM tgl_c WHERE cid = 10;

-- XVAL 05: FK SET NULL / SET DEFAULT actions (sibling of 03's CASCADE-only
-- shape). Child default 0 targets the live pid=0 parent row, so the script
-- stays valid with enforcement both OFF (default) and ON (--fk-on variant).
-- FK-off: parent writes touch only their target. FK-on: the DELETE expands
-- to a child UPDATE (SET NULL) and the UPDATE to a child UPDATE (SET
-- DEFAULT), matching the engine's authorizer output (Enforcement.hs).
CREATE TABLE snd_p (
    pid INTEGER PRIMARY KEY,
    pname TEXT NOT NULL
);
CREATE TABLE snd_c (
    cid INTEGER PRIMARY KEY,
    pid INTEGER DEFAULT 0 REFERENCES snd_p(pid) ON DELETE SET NULL ON UPDATE SET DEFAULT,
    cname TEXT NOT NULL
);
INSERT INTO snd_p VALUES (0, 'zero'), (1, 'one'), (2, 'two');
INSERT INTO snd_c VALUES (10, 1, 'a'), (20, 2, 'b'), (30, 0, 'c');
DELETE FROM snd_p WHERE pid = 1;
UPDATE snd_p SET pid = 7 WHERE pid = 2;
SELECT cid, pid FROM snd_c WHERE pid IS NULL;
SELECT cid, pid FROM snd_c WHERE cid = 20;

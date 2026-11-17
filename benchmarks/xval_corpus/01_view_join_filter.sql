-- XVAL 01: standalone non-aggregate view over a join + queries over it.
-- Pins definition-time attribution (R-schema-ddl) and view-use reads
-- (F4 expansion) without the aggregate fencing of seeds/06.
CREATE TABLE authors (
    aid INTEGER PRIMARY KEY,
    name TEXT NOT NULL
);
CREATE TABLE books (
    bid INTEGER PRIMARY KEY,
    aid INTEGER NOT NULL,
    title TEXT NOT NULL
);
INSERT INTO authors (aid, name) VALUES (1, 'Austen'), (2, 'Orwell');
INSERT INTO books (bid, aid, title) VALUES
    (11, 1, 'Emma'),
    (12, 2, '1984'),
    (13, 1, 'Persuasion');
CREATE VIEW v_book_authors AS
SELECT b.bid, b.title, a.name
FROM books b JOIN authors a ON a.aid = b.aid
WHERE b.bid > 10;
SELECT bid, title FROM v_book_authors WHERE name LIKE 'A%';
SELECT v.title, a.name
FROM v_book_authors v JOIN authors a ON a.name = v.name
WHERE v.bid < 100;

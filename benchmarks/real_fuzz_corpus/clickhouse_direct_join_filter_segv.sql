CREATE TABLE lft (id UInt64) ENGINE = MergeTree ORDER BY id;
INSERT INTO lft VALUES (1), (2), (3);
CREATE TABLE rgt (id UInt64, value String) ENGINE = MergeTree ORDER BY id;
INSERT INTO rgt VALUES (1, 'a'), (2, 'b');
ALTER TABLE rgt ADD COLUMN extra UInt64 DEFAULT 0;
INSERT INTO rgt SELECT number + 100, 'c', 0 FROM numbers(50);

SELECT count() FROM (SELECT r.value FROM lft AS l INNER JOIN rgt AS r ON l.id = r.id WHERE r.id = 1)
SETTINGS join_algorithm = 'direct';

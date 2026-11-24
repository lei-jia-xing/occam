CREATE TABLE t1 (a varchar(1), b varchar(1));
CREATE TABLE t2 (pk integer, a varchar(1), b varchar(1), c date, primary key(pk));
CREATE INDEX idx1 ON t2 (b);
INSERT INTO t1 VALUES ('d','7');
INSERT INTO t2 VALUES (1,'q','7','1970-01-01');
INSERT INTO t2 VALUES (2,'l','7','1970-01-01');
CREATE TABLE insert_select AS
SELECT t2.c AS field3
FROM t1, t2
WHERE t1.b = t2.b AND t1.a <> t2.pk;

CREATE TABLE target_tbl (id INT, status TEXT);
INSERT INTO target_tbl VALUES (1, 'pending');
UPDATE target_tbl SET status = 'active';
INSERT INTO target_tbl VALUES (2, 'crash');
SELECT * FROM target_tbl WHERE status = 'crash';

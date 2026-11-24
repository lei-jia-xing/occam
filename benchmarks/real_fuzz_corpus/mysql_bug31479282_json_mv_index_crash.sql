CREATE TABLE t1 (
  id int NOT NULL,
  j JSON DEFAULT NULL,
  PRIMARY KEY (id),
  KEY j_key123 ((CAST(json_extract(j,_utf8mb4'$') AS CHAR(64) ARRAY)))
);
INSERT INTO t1 values (1000,  CONCAT('[""]'));

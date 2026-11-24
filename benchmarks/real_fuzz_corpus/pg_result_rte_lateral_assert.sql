CREATE TABLE v0 ( v1 FLOAT ) ;
WITH v1 AS ( INSERT INTO v0 VALUES ( - - - 98 ) , ( - 84 ) , ( - - - - - - - 96 ) , ( - - - 79 ) , ( - - 60 ) , ( - - - -1 ) , ( - - - 67 ) , ( - - - -2147483648 ) RETURNING * ) SELECT * FROM v0 ;
ALTER TABLE v0 NO FORCE ROW LEVEL SECURITY ;
CREATE TEMP VIEW v0 AS SELECT * FROM v0 WHERE v1 IN ( SELECT v1 FROM v0 WHERE ( v1 , v1 ) < ANY ( SELECT v1 XMLPARSE , v1 FROM v0 WHERE v1 < - - 10 ) AND v1 <= - - - 85 UNION ALL SELECT v1 FROM LATERAL COALESCE ( -1 , 27 , 0 , NULL , - - 77 ) v1 WHERE v1 IN ( SELECT * FROM v0 WHERE - - - - 22 IN ( CASE WHEN v1 >= - - 12 THEN ( - - 61 * v1 ) ELSE - - - -32768 END , ( v1 + v1 ) / - -128 , - - - 84 ) ORDER BY v1 ) ) ;
SELECT * FROM ( SELECT * FROM v0 v1 WHERE v1 < - - - - - - - 0 AND v1 IN ( - - - - 48 , - - - - 92 ) UNION ALL SELECT * FROM v0 v1 GROUP BY ROLLUP ( v1 , v1 ) , GROUPING SETS ( ROLLUP ( - - - - -1 - - - - 89 , v1 ) , CUBE ( ( v1 + - - - 0 ) , ( v1 + - - 0 ) ) ) ) v1 ;
DROP CONVERSION IF EXISTS v0 ;
CREATE TABLE v2 ( v6 INT , v5 TIMESTAMP [ ] UNIQUE , v4 INT , v3 INT UNIQUE ) ;

SET order_by_non_integer_literal=true;
CREATE TABLE v00 (c01 INT, c02 STRING);
INSERT INTO v00 (c01, c02) VALUES (0, 'abc');
SELECT DISTINCT ON ( 'string' ) 'string' ident FROM v00 AS ta03, ( SELECT c01 < 'string' ) CROSS JOIN v00 AS ta04 ASOF RIGHT JOIN v00 AS ta05 USING ( c02, c01 );

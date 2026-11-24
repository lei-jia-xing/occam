CREATE TABLE l AS SELECT g*2 AS a FROM range(200000) t(g);
CREATE TABLE r AS SELECT g*2+1 AS k, g*2+2 AS k2, [g]::INT[] AS v FROM range(2000) t(g);
CREATE TABLE o AS SELECT l.a, r.v FROM l LEFT JOIN r ON l.a >= r.k AND l.a < r.k2;

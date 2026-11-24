CREATE TABLE INT8_TBL(q1 int8, q2 int8);
INSERT INTO INT8_TBL VALUES
  ('  123   ','  456'),
  ('123   ','4567890123456789'),
  ('4567890123456789','123'),
  (+4567890123456789,'4567890123456789'),
  ('+4567890123456789','-4567890123456789');
SELECT * FROM
  int8_tbl c LEFT JOIN (
    int8_tbl a LEFT JOIN (SELECT q1, coalesce(q2,42) AS x FROM int8_tbl b) ss1
      ON a.q2 = ss1.q1
    CROSS JOIN
    LATERAL (SELECT q1, coalesce(ss1.x,q2) AS y FROM int8_tbl d) ss2
  ) ON c.q2 = ss2.q1,
  LATERAL (SELECT ss2.y OFFSET 0) ss3;

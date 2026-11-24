CREATE TABLE t(a INTEGER);
INSERT INTO t VALUES(1),(2),(3);
SELECT 1 = (SELECT 1
            FROM (SELECT 1
                  FROM t LEFT JOIN
                         (SELECT 1
                          FROM t
                          WHERE NOT EXISTS (SELECT 1
                                            FROM t
                                            WINDOW w1 AS(PARTITION BY a)
                                           )
                         ) AS x
                         ON 1 > 2
                  ) AS z
           );

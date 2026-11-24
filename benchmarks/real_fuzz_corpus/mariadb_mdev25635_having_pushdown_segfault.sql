CREATE TABLE randgen_ykc (
randgen_l2i8sidec INTEGER,
randgen_xq82b TEXT,
randgen_lfd1o2avn INTEGER,
randgen_pnnh INTEGER,
randgen_f072 TEXT,
randgen_zdoqx5m8 INTEGER,
randgen_xt7yer INTEGER,
randgen_wpme INTEGER,
PRIMARY KEY (randgen_l2i8sidec)
);

alter table randgen_ykc rename to randgen_dm2_kr7my;

WITH
jennifer_0 AS (select
    ref_0.randgen_xt7yer as c2,
    count(
    cast(38.11 as SIGNED)) as c12
  from
    randgen_dm2_kr7my as ref_0
  group by ref_0.randgen_xt7yer)
select
    subq_0.c2 as c0
  from
    (select
          ref_3.c2 as c0,
          ref_3.c12 as c1,
          ref_3.c2 as c2
        from
          (jennifer_0 as ref_3
            cross join randgen_dm2_kr7my as ref_4
            )
    ) as subq_0
  where subq_0.c1 > subq_0.c0;

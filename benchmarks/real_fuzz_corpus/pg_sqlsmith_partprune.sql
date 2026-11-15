DROP TABLE IF EXISTS public.actualizar_sistema;
DROP TABLE IF EXISTS public.anexos;
DROP TABLE IF EXISTS public.radicado;

CREATE TABLE public.actualizar_sistema (
    actu_codi integer NOT NULL,
    sentencia character varying,
    sentencia_verificacion character varying,
    estado smallint DEFAULT 0,
    observacion character varying,
    svn character varying,
    num_registros_total bigint DEFAULT 0,
    num_registros_restantes bigint DEFAULT 0,
    num_registros_bloque integer DEFAULT 0
);

CREATE TABLE public.anexos (
    anex_radi_nume numeric(20,0) NOT NULL,
    anex_codigo character varying(50) NOT NULL,
    anex_tipo smallint NOT NULL,
    anex_desc character varying(512),
    anex_numero numeric(5,0) NOT NULL,
    anex_path character varying(200),
    anex_borrado character varying(1) NOT NULL,
    anex_fecha timestamp with time zone,
    anex_nombre character varying(100),
    anex_usua_codi integer,
    anex_tamano numeric,
    anex_fisico smallint DEFAULT 0,
    anex_fecha_firma timestamp with time zone,
    anex_datos_firma character varying,
    arch_codi bigint DEFAULT 0,
    arch_codi_firma bigint DEFAULT 0
);


CREATE TABLE public.radicado (
    radi_nume_temp numeric(20,0) NOT NULL,
    radi_fech_radi timestamp with time zone NOT NULL,
    radi_texto integer,
    radi_usua_ante integer,
    radi_usua_radi integer,
    radi_inst_actu integer,
    radi_imagen character varying(50)
)
PARTITION BY RANGE (radi_fech_radi);

CREATE TABLE public.radicado2012 PARTITION OF public.radicado
FOR VALUES FROM ('2012-01-01 00:00:00-05') TO ('2013-01-01 00:00:00-05');

CREATE TABLE public.radicado2013 PARTITION OF public.radicado
FOR VALUES FROM ('2013-01-01 00:00:00-05') TO ('2014-01-01 00:00:00-05')
PARTITION BY HASH (radi_inst_actu);

CREATE TABLE public.radicado2013_part00 PARTITION OF public.radicado2013
FOR VALUES WITH (modulus 2, remainder 0);

CREATE TABLE public.radicado2013_part01 PARTITION OF public.radicado2013
FOR VALUES WITH (modulus 2, remainder 1);

CREATE TABLE public.radicado2014 PARTITION OF public.radicado
FOR VALUES FROM ('2014-01-01 00:00:00-05') TO ('2015-01-01 00:00:00-05')
PARTITION BY HASH (radi_inst_actu);

CREATE TABLE public.radicado2014_part00 PARTITION OF public.radicado2014
FOR VALUES WITH (modulus 2, remainder 0);

CREATE TABLE public.radicado2014_part01 PARTITION OF public.radicado2014
FOR VALUES WITH (modulus 2, remainder 1);

CREATE TABLE public.radicado2015 PARTITION OF public.radicado
FOR VALUES FROM ('2015-01-01 00:00:00-05') TO ('2016-01-01 00:00:00-05')
PARTITION BY HASH (radi_inst_actu);

CREATE TABLE public.radicado2015_part00 PARTITION OF public.radicado2015
FOR VALUES WITH (modulus 2, remainder 0);

CREATE TABLE public.radicado2015_part01 PARTITION OF public.radicado2015
FOR VALUES WITH (modulus 2, remainder 1);

CREATE TABLE public.radicado2016 PARTITION OF public.radicado
FOR VALUES FROM ('2016-01-01 00:00:00-05') TO ('2017-01-01 00:00:00-05')
PARTITION BY HASH (radi_inst_actu);

CREATE TABLE public.radicado2016_part00 PARTITION OF public.radicado2016
FOR VALUES WITH (modulus 2, remainder 0);

CREATE TABLE public.radicado2016_part01 PARTITION OF public.radicado2016
FOR VALUES WITH (modulus 2, remainder 1);

CREATE TABLE public.radicado2017 PARTITION OF public.radicado
FOR VALUES FROM ('2017-01-01 00:00:00-05') TO ('2018-01-01 00:00:00-05');

	
insert into radicado 
select (random() * 51245787878787878787)::numeric(20), '2012-01-01'::date + ((random()*1800) || ' days')::interval, 
       random()*25000, random()*25000, random()*25000, random()*25000,md5((random()*51245787878787878787)::text) 
  from generate_series(1, 1000000);

select  
  (select pg_catalog.avg(actu_codi) from public.actualizar_sistema)
     as c0, 
  sample_0.radi_nume_temp as c1, 
  sample_0.radi_usua_ante as c2, 
  (select anex_borrado from public.anexos limit 1 offset 4)
     as c3, 
  pg_catalog.pg_stat_get_buf_alloc() as c4
from 
  public.radicado as sample_0 tablesample bernoulli (9.7) 
where (sample_0.radi_texto = cast(nullif(sample_0.radi_inst_actu, sample_0.radi_usua_radi) as int4)) 
   or (cast(null as jsonb) ?& case when sample_0.radi_imagen is not NULL then cast(null as _text) else cast(null as _text) end)
limit 33;

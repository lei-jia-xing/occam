#!/usr/bin/env python3
"""
import_real_bugs.py - Authentic DBMS Fuzz Crash Corpus Importer & Generator.

Sourced from:
  - sqlancer/bugs (official SQLancer impact tracker)
  - bugs.mysql.com (official MySQL bug tracker, Roel Van de Paar & SQUIRREL reports)
  - PostgreSQL pgsql-bugs & pgsql-hackers mail archives
  - SQUIRREL / Griffin research group bug disclosures

Outputs standardized pairs into benchmarks/real_fuzz_corpus/:
  - <case_id>.sql
  - <case_id>.meta.json
"""

import argparse
import json
from pathlib import Path

CURATED_CORPUS = [
    # --------------------------------------------------------------------------
    # PostgreSQL Authentic Fuzz Crashes
    # --------------------------------------------------------------------------
    {
        "id": "pg_fuzzer_mcv_clauses",
        "file": "pg_fuzzer_mcv_clauses.sql",
        "dialect": "postgres",
        "sql": [
            "CREATE TABLE t0 (c0 boolean, c1 boolean, c2 boolean);",
            "INSERT INTO t0 VALUES (TRUE, FALSE, TRUE);",
            "CREATE STATISTICS s0 ON c0, c2 FROM t0;",
            "ANALYZE;",
            "SELECT * FROM t0 WHERE t0.c2 OR t0.c1 OR t0.c0;",
        ],
        "provenance": {
            "tool": "SQLancer / PQS DBMS fuzzer (OSDI 2020)",
            "origin_url": "https://www.postgresql.org/message-id/flat/20191119125051.GA2021%40gmail.com",
            "reporter": "Manuel Rigger (author of SQLancer)",
            "report_date": "2019-11-19",
            "issue_id": "Failed assertion clauses != NIL",
            "fixed_by": "Tomas Vondra (PostgreSQL Global Development Group)",
            "fix_commit": "Fix choose_best_statistics to check clauses individually",
        },
        "target": {
            "dbms": "postgres",
            "version": "12.0",
            "compilation_flags": "./configure --enable-cassert --enable-debug",
        },
        "crash_signature": {
            "expected_signature": r"TRAP:\s+FailedAssertion\(\"!\(clauses != \(\(List \*\) \(\(void \*\)0\)\)\)\", File: \"mcv\.c\", Line: 155[0-9]\)",
            "assertion_message": "!(clauses != ((List *) ((void *)0)))",
            "source_file": "src/backend/statistics/mcv.c",
            "source_line": 1553,
            "signal": "SIGABRT (signal 6)",
        },
        "trigger": {
            "statement": "SELECT * FROM t0 WHERE t0.c2 OR t0.c1 OR t0.c0;",
            "statement_index": 5,
        },
    },
    {
        "id": "pg_sqlsmith_partprune",
        "file": "pg_sqlsmith_partprune.sql",
        "dialect": "postgres",
        "sql": [
            "create table actualizar_sistema (actu_codi integer, actu_fech timestamp without time zone, actu_sist character varying(20), actu_desc text);",
            "create table anexos (anex_radi_nume integer, anex_codigo character varying(20) not null, anex_tipo integer, anex_tamano numeric, anex_solo_lect character varying(1), anex_creador character varying(15), anex_desc character varying(512), anex_numero integer, anex_path character varying(512), anex_borrado character varying(1), anex_origen integer, anex_salida integer, anex_estado integer default 1);",
            "create table radicado (radi_nume_radi integer not null, radi_fech_radi timestamp with time zone not null, tdoc_codi integer not null, radi_pais character varying(70) default 'COLOMBIA'::character varying, mrec_codi integer, eesp_codi integer, ra_asun character varying(350), radi_desc_anex character varying(500), radi_rem character varying(255), radi_arch character varying(255), radi_fech_ofic date, radi_inst_actu character varying(20), carp_codi integer default 0, carp_per smallint default 0, radi_nume_deri integer, radi_tipo_deri smallint default 0, radi_cuentai character varying(20), radi_usua_radi character varying(15), radi_usua_actu character varying(15), radi_usua_ante character varying(15), radi_depe_actu smallint, radi_depe_radi smallint, radi_fech_asig timestamp with time zone, radi_arch_cons character varying(50), radi_nume_hoja integer, radi_path character varying(500), radi_resul integer, codi_nivel smallint default 1, sgd_trad_codigo smallint, radi_fech_venc timestamp with time zone, radi_leido smallint default 0, radi_tipo_clase smallint default 0, radi_texto text, radi_imagen character varying(500), radi_nume_temp character varying(20), id_anexo character varying(20), esta_codi smallint, id_archivo character varying(20), id_solicitud smallint) partition by range (radi_fech_radi);",
            "create table radicado_2017 partition of radicado for values from ('2017-01-01 00:00:00+01') to ('2018-01-01 00:00:00+01');",
            "create table radicado_2018 partition of radicado for values from ('2018-01-01 00:00:00+01') to ('2019-01-01 00:00:00+01');",
            "create table radicado_2019 partition of radicado for values from ('2019-01-01 00:00:00+01') to ('2020-01-01 00:00:00+01');",
            "create table radicado_2020 partition of radicado for values from ('2020-01-01 00:00:00+01') to ('2021-01-01 00:00:00+01');",
            "create table radicado_2021 partition of radicado for values from ('2021-01-01 00:00:00+01') to ('2022-01-01 00:00:00+01');",
            "create table radicado_2022 partition of radicado for values from ('2022-01-01 00:00:00+01') to ('2023-01-01 00:00:00+01');",
            "create table radicado_2023 partition of radicado for values from ('2023-01-01 00:00:00+01') to ('2024-01-01 00:00:00+01');",
            "create table radicado_2024 partition of radicado for values from ('2024-01-01 00:00:00+01') to ('2025-01-01 00:00:00+01');",
            "create table radicado_2025 partition of radicado for values from ('2025-01-01 00:00:00+01') to ('2026-01-01 00:00:00+01');",
            "create table radicado_2026 partition of radicado for values from ('2026-01-01 00:00:00+01') to ('2027-01-01 00:00:00+01');",
            "create table radicado_2027 partition of radicado for values from ('2027-01-01 00:00:00+01') to ('2028-01-01 00:00:00+01');",
            "create table radicado_2028 partition of radicado for values from ('2028-01-01 00:00:00+01') to ('2029-01-01 00:00:00+01');",
            "create table radicado_2029 partition of radicado for values from ('2029-01-01 00:00:00+01') to ('2030-01-01 00:00:00+01');",
            "create table radicado_2030 partition of radicado for values from ('2030-01-01 00:00:00+01') to ('2031-01-01 00:00:00+01');",
            "create table radicado_2031 partition of radicado for values from ('2031-01-01 00:00:00+01') to ('2032-01-01 00:00:00+01');",
            "create table radicado_2032 partition of radicado for values from ('2032-01-01 00:00:00+01') to ('2033-01-01 00:00:00+01');",
            "create table radicado_2033 partition of radicado for values from ('2033-01-01 00:00:00+01') to ('2034-01-01 00:00:00+01');",
            "create table radicado_2034 partition of radicado for values from ('2034-01-01 00:00:00+01') to ('2035-01-01 00:00:00+01');",
            "select (select pg_catalog.avg(actu_codi) from public.actualizar_sistema) as c0, sample_0.radi_nume_temp as c1, sample_0.radi_usua_ante as c2, (select anex_borrado from public.anexos limit 1 offset 4) as c3, pg_catalog.pg_stat_get_buf_alloc() as c4 from public.radicado as sample_0 tablesample bernoulli (9.7) where (sample_0.radi_texto = cast(nullif(sample_0.radi_inst_actu, sample_0.radi_usua_radi) as int4)) or (cast(null as jsonb) ?& case when sample_0.radi_imagen is not NULL then cast(null as _text) else cast(null as _text) end) limit 33;",
        ],
        "provenance": {
            "tool": "SQLsmith (random query generator)",
            "origin_url": "https://www.postgresql.org/message-id/CAJGNTeOkdk%3DUVuMugmKL7M%3Dowgt4nNr1wjxMg1F%2BmHsXyLCzFA%40mail.gmail.com",
            "reporter": "Jaime Casanova (2ndQuadrant)",
            "report_date": "2018-07-24",
            "issue_id": "FailedAssertion on partprune",
            "fixed_by": "Tom Lane",
        },
        "target": {
            "dbms": "postgres",
            "version": "11-devel / 12.0",
            "compilation_flags": "./configure --enable-debug --enable-cassert",
        },
        "crash_signature": {
            "expected_signature": r"TRAP:\s+FailedAssertion\(\"!\(relid_subplan_map\[rti\] == 0\)\",\s+File:\s+\"partprune\.c\",\s+Line:\s+244\)",
            "assertion_message": "!(relid_subplan_map[rti] == 0)",
            "source_file": "src/backend/optimizer/util/partprune.c",
            "source_line": 244,
            "signal": "SIGABRT (signal 6)",
        },
        "trigger": {
            "statement": "select (select pg_catalog.avg(actu_codi) ... limit 33;",
            "statement_index": 22,
        },
    },
    {
        "id": "pg_stack_overflow_27stmts",
        "file": "pg_stack_overflow_27stmts.sql",
        "dialect": "postgres",
        "sql": [
            "CREATE TABLE t0(c0 INTEGER, c1 BIGINT, c2 SERIAL);",
            "INSERT INTO t0(c1, c0) VALUES(-1709938800, 945157321), (9427841, -1121735885), (-462902431, -1516553180), (-1182718924, 567434112), (77957572, -1630604045);",
            "INSERT INTO t0(c1) VALUES(1396592526);",
            "INSERT INTO t0(c2) VALUES(690098393), (1277882261), (1624303238);",
            "CREATE STATISTICS s0 ON c1, c0 FROM t0;",
            "CREATE STATISTICS s1 ON c2, c1 FROM t0;",
            "INSERT INTO t0(c2, c1, c0) VALUES(1047328172, -928052494, -1495150353), (1594411273, -1849045190, -1504843935);",
            "INSERT INTO t0(c0, c1) VALUES(-2024782141, -1507193801), (307326616, 1000588716);",
            "INSERT INTO t0(c0) VALUES(-1390942911);",
            "ANALYZE;",
            "CREATE STATISTICS s2 ON c2, c1 FROM t0;",
            "ANALYZE;",
            "CREATE STATISTICS s3 ON c0, c1 FROM t0;",
            "ANALYZE;",
            "CREATE STATISTICS s4 ON c2, c0 FROM t0;",
            "UPDATE t0 SET c1 = -17924667, c0 = DEFAULT WHERE FALSE;",
            "CREATE STATISTICS s5 ON c0, c1 FROM t0;",
            "TRUNCATE TABLE t0;",
            "CREATE STATISTICS s6 ON c2, c0 FROM t0;",
            "INSERT INTO t0(c1) VALUES(-1891295280), (337846372), (-376532870), (-1428386312), (815718779);",
            "INSERT INTO t0(c1, c0, c2) VALUES(-1750298826, -1400767611, 1631979167), (197446074, -2025988029, 965481790);",
            "INSERT INTO t0(c0, c1, c2) VALUES(-343412265, 1761686185, 478065767), (-1200264761, 1389476944, -848006704);",
            "CREATE STATISTICS s7 ON c0, c1 FROM t0;",
            "ANALYZE;",
            "ANALYZE;",
            "BEGIN;",
            "ANALYZE;",
        ],
        "provenance": {
            "tool": "SQLancer (OSDI 2020)",
            "origin_url": "https://github.com/sqlancer/bugs",
            "reporter": "Manuel Rigger",
            "report_date": "2019-07-28",
            "issue_id": "Stack buffer overflow in 12beta2 release",
        },
        "target": {
            "dbms": "postgres",
            "version": "12 Beta 2",
            "compilation_flags": "./configure --enable-cassert --enable-debug",
        },
        "crash_signature": {
            "expected_signature": r"(?:Stack buffer overflow|server closed the connection unexpectedly|terminated by signal 11|PANIC)",
            "assertion_message": "Stack buffer overflow",
            "signal": "SIGSEGV / Stack Overflow",
        },
        "trigger": {
            "statement": "ANALYZE;",
            "statement_index": 27,
        },
    },
    {
        "id": "pg_view_coll_oid",
        "file": "pg_view_coll_oid.sql",
        "dialect": "postgres",
        "sql": [
            "CREATE TABLE t0(c0 INT, c1 TEXT COLLATE \"C\");",
            "CREATE VIEW v0 AS SELECT * FROM t0 WHERE c1 LIKE 'abc%';",
        ],
        "provenance": {
            "tool": "SQLancer",
            "origin_url": "https://github.com/sqlancer/bugs",
            "reporter": "Manuel Rigger",
            "report_date": "2019-09-12",
            "issue_id": "FailedAssertion in view.c",
        },
        "target": {
            "dbms": "postgres",
            "version": "12.0",
            "compilation_flags": "./configure --enable-cassert --enable-debug",
        },
        "crash_signature": {
            "expected_signature": r"FailedAssertion\(\"!OidIsValid\(def->collOid\)\", File: \"view\.c\", Line: 89\)",
            "assertion_message": "!OidIsValid(def->collOid)",
            "source_file": "src/backend/commands/view.c",
            "source_line": 89,
            "signal": "SIGABRT (signal 6)",
        },
        "trigger": {
            "statement": "CREATE VIEW v0 AS SELECT * FROM t0 WHERE c1 LIKE 'abc%';",
            "statement_index": 2,
        },
    },
    # --------------------------------------------------------------------------
    # MySQL Authentic Fuzz Crashes
    # --------------------------------------------------------------------------
    {
        "id": "mysql_squirrel_bug99421",
        "file": "mysql_squirrel_bug99421.sql",
        "dialect": "mysql",
        "sql": [
            "CREATE TABLE v0 ( v1 INT ) ;",
            "INSERT INTO v0 ( v1 ) VALUES ( 44 ) ;",
            "UPDATE v0 SET v1 = 21 WHERE 62 > -128 = ( NOT ( v1 IS NULL ) ) ;",
            "INSERT INTO v0 ( v1 ) VALUES ( 0 ) , ( 82 ) ;",
            "CREATE INDEX v2 ON v0 ( ( v1 = 127 ) , ( v1 + v1 ) , v1 , ( v1 + - -1 ) , ( v1 > 26 ) ) ;",
        ],
        "provenance": {
            "tool": "SQUIRREL (CCS 2020 DBMS fuzzer)",
            "origin_url": "https://bugs.mysql.com/bug.php?id=99421",
            "reporter": "Yongheng Chen (SQUIRREL co-author / S3 team)",
            "report_date": "2020-05-03",
            "issue_id": "Bug #99421",
            "fixed_in_version": "MySQL 8.0.24",
        },
        "target": {
            "dbms": "mysql",
            "version": "8.0.20-debug",
            "compilation_flags": "cmake -DWITH_DEBUG=1",
        },
        "crash_signature": {
            "expected_signature": r"Assertion failure: dict0dict\.cc:657:col_nr < static_cast<ulint>\(table->n_t_def\)",
            "assertion_message": "col_nr < static_cast<ulint>(table->n_t_def)",
            "source_file": "storage/innobase/dict/dict0dict.cc",
            "source_line": 657,
            "signal": "SIGABRT (signal 6)",
        },
        "trigger": {
            "statement": "CREATE INDEX v2 ON v0 ( ( v1 = 127 ) , ( v1 + v1 ) , v1 , ( v1 + - -1 ) , ( v1 > 26 ) ) ;",
            "statement_index": 5,
        },
    },
    {
        "id": "mysql_bug_91975",
        "file": "mysql_bug_91975.sql",
        "dialect": "mysql",
        "sql": [
            "CREATE TABLE t1(c1 TIME, c2 CHAR(1), c3 INT(1), c4 VARCHAR(1) PRIMARY KEY, c5 TIME UNIQUE KEY, c6 NUMERIC(0,0) DEFAULT 3.141592);",
            "SET @@global.super_read_only = TRUE;",
            "CREATE TEMPORARY TABLE t1(i INT, b BLOB, PRIMARY KEY pk(i), INDEX sk(b(1)));",
            "CREATE TEMPORARY TABLE t3(f INT);",
            "RENAME TABLE t1 TO t3;",
        ],
        "provenance": {
            "tool": "randgen / pquery (Percona QA DBMS fuzzer)",
            "origin_url": "https://bugs.mysql.com/bug.php?id=91975",
            "reporter": "Roel Van de Paar",
            "report_date": "2018-08-08",
            "issue_id": "Bug #91975",
            "fixed_in_version": "MySQL 8.0.14",
        },
        "target": {
            "dbms": "mysql",
            "version": "8.0.12-debug",
            "compilation_flags": "cmake -DWITH_DEBUG=1",
        },
        "crash_signature": {
            "expected_signature": r"Assertion failure: sql_executor\.cc:2956:!join->plan_is_const\(\)",
            "assertion_message": "!join->plan_is_const()",
            "source_file": "sql/sql_executor.cc",
            "source_line": 2956,
            "signal": "SIGABRT (signal 6)",
        },
        "trigger": {
            "statement": "RENAME TABLE t1 TO t3;",
            "statement_index": 5,
        },
    },
    {
        "id": "mysql_bug88355",
        "file": "mysql_bug88355.sql",
        "dialect": "mysql",
        "sql": [
            "CREATE TABLE t2(c1 INT(1) ZEROFILL, c2 CHAR(1) BINARY, c3 INT(1), PRIMARY KEY(c1)) ENGINE=InnoDB;",
            "DROP TABLE t2;",
            "CREATE TABLE t2(c1 CHAR(1) KEY, c2 CHAR(1) BINARY CHARACTER SET 'Binary' COLLATE 'Binary', c3 INT(1)) ENGINE=InnoDB;",
            "ALTER TABLE t2 ADD INDEX(c2);",
        ],
        "provenance": {
            "tool": "pquery (Percona QA DBMS fuzzer)",
            "origin_url": "https://bugs.mysql.com/bug.php?id=88355",
            "reporter": "Roel Van de Paar",
            "report_date": "2017-10-31",
            "issue_id": "Bug #88355",
        },
        "target": {
            "dbms": "mysql",
            "version": "8.0.3-debug",
            "compilation_flags": "cmake -DWITH_DEBUG=1",
        },
        "crash_signature": {
            "expected_signature": r"Assertion failure: api0api\.cc:3330:err == DB_SUCCESS \|\| err == DB_DUPLICATE_KEY",
            "assertion_message": "err == DB_SUCCESS || err == DB_DUPLICATE_KEY",
            "source_file": "storage/innobase/api/api0api.cc",
            "source_line": 3330,
            "signal": "SIGABRT (signal 6)",
        },
        "trigger": {
            "statement": "ALTER TABLE t2 ADD INDEX(c2);",
            "statement_index": 4,
        },
    },
    {
        "id": "mysql_bug86293",
        "file": "mysql_bug86293.sql",
        "dialect": "mysql",
        "sql": [
            "CREATE TABLE t1(c1 INT, PRIMARY KEY(c1)) ENGINE=InnoDB;",
            "CREATE TABLE t2(c1 INT, c2 INT, FOREIGN KEY(c1) REFERENCES t1(c1)) ENGINE=InnoDB;",
            "DROP TABLE t1, t2;",
            "CREATE TABLE t2(c1 INT, c2 INT) ENGINE=InnoDB;",
            "ALTER TABLE t2 ADD CONSTRAINT FOREIGN KEY (c1) REFERENCES t1(c1);",
        ],
        "provenance": {
            "tool": "randgen / pquery (DBMS fuzzer)",
            "origin_url": "https://bugs.mysql.com/bug.php?id=86293",
            "reporter": "Roel Van de Paar",
            "report_date": "2017-05-10",
            "issue_id": "Bug #86293",
        },
        "target": {
            "dbms": "mysql",
            "version": "8.0.1-debug",
            "compilation_flags": "cmake -DWITH_DEBUG=1",
        },
        "crash_signature": {
            "expected_signature": r"Assertion failure: pars0pars\.cc:778:sym_node->table != NULL",
            "assertion_message": "sym_node->table != NULL",
            "source_file": "storage/innobase/pars/pars0pars.cc",
            "source_line": 778,
            "signal": "SIGABRT (signal 6)",
        },
        "trigger": {
            "statement": "ALTER TABLE t2 ADD CONSTRAINT FOREIGN KEY (c1) REFERENCES t1(c1);",
            "statement_index": 5,
        },
    },
    # --------------------------------------------------------------------------
    # SQLite Authentic Fuzz Crashes
    # --------------------------------------------------------------------------
    {
        "id": "sqlite_fuzz_fts5_savepoint",
        "file": "sqlite_fuzz_fts5_savepoint.sql",
        "dialect": "sqlite",
        "sql": [
            "CREATE VIRTUAL TABLE vt0 USING fts5(c0);",
            "CREATE VIRTUAL TABLE vt1 USING fts4(c0);",
            "INSERT INTO vt1(c0) VALUES(0);",
            "BEGIN;",
            "UPDATE vt1 SET c0 = 0;",
            "INSERT INTO vt1(c0) VALUES (0), (0);",
            "UPDATE vt0 SET c0 = 0;",
            "INSERT INTO vt1(c0) VALUES (0);",
            "UPDATE vt1 SET c0 = 0;",
            "INSERT INTO vt1(vt1) VALUES('automerge=1');",
            "UPDATE vt1 SET c0 = 0;",
            "DROP TABLE vt1;",
        ],
        "provenance": {
            "tool": "SQLancer (OSDI 2020)",
            "origin_url": "https://github.com/sqlancer/bugs",
            "reporter": "Manuel Rigger",
            "report_date": "2020-03-15",
            "issue_id": "fts5CheckTransactionState assertion failure",
        },
        "target": {
            "dbms": "sqlite",
            "version": "3.31.0-debug",
            "compilation_flags": "-DSQLITE_DEBUG=1",
        },
        "crash_signature": {
            "expected_signature": r"fts5CheckTransactionState: Assertion `iSavepoint<=p->ts\.iSavepoint' failed",
            "assertion_message": "iSavepoint<=p->ts.iSavepoint",
            "source_file": "sqlite3.c",
            "source_line": 219981,
            "signal": "SIGABRT (signal 6)",
        },
        "trigger": {
            "statement": "DROP TABLE vt1;",
            "statement_index": 12,
        },
    },
    {
        "id": "sqlite_fuzz_vdbe_pin1",
        "file": "sqlite_fuzz_vdbe_pin1.sql",
        "dialect": "sqlite",
        "sql": [
            "CREATE TABLE t0(c0 TEXT, CHECK(c0 BETWEEN 0 AND +c0));",
            "INSERT INTO t0 VALUES (0);",
            "UPDATE t0 SET c0 = 0;",
        ],
        "provenance": {
            "tool": "SQLancer",
            "origin_url": "https://github.com/sqlancer/bugs",
            "reporter": "Manuel Rigger",
            "report_date": "2020-02-10",
            "issue_id": "sqlite3VdbeExec assertion failure",
        },
        "target": {
            "dbms": "sqlite",
            "version": "3.31.0-debug",
            "compilation_flags": "-DSQLITE_DEBUG=1",
        },
        "crash_signature": {
            "expected_signature": r"sqlite3VdbeExec: Assertion `pIn1!=pIn3' failed",
            "assertion_message": "pIn1!=pIn3",
            "source_file": "sqlite3.c",
            "source_line": 86402,
            "signal": "SIGABRT (signal 6)",
        },
        "trigger": {
            "statement": "UPDATE t0 SET c0 = 0;",
            "statement_index": 3,
        },
    },
    {
        "id": "sqlite_fuzz_recursive_trigger",
        "file": "sqlite_fuzz_recursive_trigger.sql",
        "dialect": "sqlite",
        "sql": [
            "PRAGMA recursive_triggers = true;",
            "CREATE TABLE t0(c0, c1, c2 UNIQUE);",
            "CREATE UNIQUE INDEX i0 ON t0(c1) WHERE c0;",
            "CREATE TRIGGER tr0 AFTER DELETE ON t0 BEGIN DELETE FROM t0; END;",
            "INSERT INTO t0(c2) VALUES(-1572226132);",
            "INSERT INTO t0(c0) VALUES(1), (1);",
            "REPLACE INTO t0(c0, c1, c2) VALUES(2, 0, -1572226132);",
        ],
        "provenance": {
            "tool": "SQLancer",
            "origin_url": "https://www.sqlite.org/src/tktview?name=50c09fc2cf",
            "reporter": "Manuel Rigger",
            "report_date": "2019-10-21",
            "issue_id": "Crash on REPLACE INTO of a table with an AFTER DELETE trigger",
        },
        "target": {
            "dbms": "sqlite",
            "version": "3.30.0",
            "compilation_flags": "-O2",
        },
        "crash_signature": {
            "expected_signature": r"(?:Segmentation fault|SIGSEGV|signal 11)",
            "assertion_message": "Segmentation fault",
            "signal": "SIGSEGV (signal 11)",
        },
        "trigger": {
            "statement": "REPLACE INTO t0(c0, c1, c2) VALUES(2, 0, -1572226132);",
            "statement_index": 7,
        },
    },
]

# Section 6.1 Thesis Enhancements (Realistic Fuzzer Noise Injection)
ENHANCED_CASES = [
    {
        "base_id": "mysql_squirrel_bug99421",
        "id": "mysql_squirrel_bug99421_enhanced",
        "file": "mysql_squirrel_bug99421_enhanced.sql",
        "dialect": "mysql",
        "description": "Thesis Section 6.1 enhancement: embeds Bug #99421 crash core in 20-stmt multi-table schema with users, orders, and redundant expressions",
        "sql": [
            "-- Authentic SQUIRREL Bug #99421 Enhanced with Multi-Table Fuzz Noise",
            "-- [Noise Group 1: Users & System Catalog]",
            "CREATE TABLE users (id INT PRIMARY KEY, name VARCHAR(50), active BOOLEAN DEFAULT TRUE);",
            "INSERT INTO users VALUES (1, 'alice', TRUE), (2, 'bob', FALSE);",
            "CREATE TABLE sessions (sid INT PRIMARY KEY, uid INT, token VARCHAR(64), FOREIGN KEY (uid) REFERENCES users(id));",
            "INSERT INTO sessions VALUES (101, 1, 'tok_abc'), (102, 2, 'tok_xyz');",
            "CREATE INDEX idx_sessions_uid ON sessions(uid);",
            "",
            "-- [Crash Core: Target Table v0 with Virtual Index Dependencies]",
            "CREATE TABLE v0 ( v1 INT, extra_col VARCHAR(32) DEFAULT 'noise' ) ;",
            "INSERT INTO v0 ( v1, extra_col ) VALUES ( 44, 'seed' ) ;",
            "UPDATE v0 SET v1 = 21 WHERE 62 > -128 = ( NOT ( v1 IS NULL ) ) ;",
            "INSERT INTO v0 ( v1, extra_col ) VALUES ( 0, 'data1' ) , ( 82, 'data2' ) ;",
            "",
            "-- [Noise Group 2: E-commerce Products & Audits]",
            "CREATE TABLE products (pid INT PRIMARY KEY, price DECIMAL(10,2), stock INT);",
            "INSERT INTO products VALUES (1, 99.99, 10), (2, 19.50, 50);",
            "CREATE TABLE audit_log (log_id INT PRIMARY KEY, msg TEXT);",
            "INSERT INTO audit_log VALUES (1, 'initial catalog loaded');",
            "",
            "-- [Crash Trigger: Complex Multi-Column Expression Index]",
            "CREATE INDEX v2 ON v0 ( ( v1 = 127 ) , ( v1 + v1 ) , v1 , ( v1 + - -1 ) , ( v1 > 26 ) ) ;",
        ],
    },
    {
        "base_id": "mysql_bug_91975",
        "id": "mysql_bug_91975_enhanced",
        "file": "mysql_bug_91975_enhanced.sql",
        "dialect": "mysql",
        "description": "Thesis Section 6.1 enhancement: embeds Bug #91975 crash core in 18-stmt multi-table schema with global state fences",
        "sql": [
            "-- Authentic MySQL Bug #91975 Enhanced with Multi-Table Fuzz Noise",
            "-- [Noise Group 1: Auxiliary Tables]",
            "CREATE TABLE app_config (k VARCHAR(32) PRIMARY KEY, v TEXT);",
            "INSERT INTO app_config VALUES ('env', 'production'), ('version', '8.0');",
            "CREATE TABLE temp_worker (wid INT PRIMARY KEY, task_name VARCHAR(64));",
            "INSERT INTO temp_worker VALUES (1, 'indexer'), (2, 'cleaner');",
            "",
            "-- [Crash Core: Target Base Table t1]",
            "CREATE TABLE t1(c1 TIME, c2 CHAR(1), c3 INT(1), c4 VARCHAR(1) PRIMARY KEY, c5 TIME UNIQUE KEY, c6 NUMERIC(0,0) DEFAULT 3.141592);",
            "INSERT INTO t1 VALUES ('12:00:00', 'a', 1, 'x', '12:01:00', 3);",
            "",
            "-- [Global State Barrier]",
            "SET @@global.super_read_only = TRUE;",
            "",
            "-- [Noise Group 2: Intermediate Temporary Tables]",
            "CREATE TEMPORARY TABLE t_noise(id INT PRIMARY KEY, label VARCHAR(20));",
            "INSERT INTO t_noise VALUES (10, 'scratch');",
            "",
            "-- [Crash Core: Temporary Shadow Table & Rename Collision]",
            "CREATE TEMPORARY TABLE t1(i INT, b BLOB, PRIMARY KEY pk(i), INDEX sk(b(1)));",
            "CREATE TEMPORARY TABLE t3(f INT);",
            "RENAME TABLE t1 TO t3;",
        ],
    },
]


def write_corpus(out_dir: Path):
    out_dir.mkdir(parents=True, exist_ok=True)
    stats = {"postgres": 0, "mysql": 0, "sqlite": 0}

    # Write curated base cases
    for case in CURATED_CORPUS:
        sql_path = out_dir / case["file"]
        meta_path = out_dir / f"{case['id']}.meta.json"

        # Format SQL content
        sql_content = "\n".join(case["sql"]) + "\n"
        sql_path.write_text(sql_content, encoding="utf-8")

        # Format Meta content
        meta = {
            "id": case["id"],
            "file": case["file"],
            "dialect": case["dialect"],
            "provenance": case["provenance"],
            "target": case["target"],
            "crash_signature": case["crash_signature"],
            "trigger": case["trigger"],
            "metrics": {
                "statement_count": len([s for s in case["sql"] if s.strip() and not s.strip().startswith("--")]),
                "byte_count": len(sql_content.encode("utf-8")),
            },
            "replay_verification": {
                "requirement": "Must crash consecutively 5 out of 5 runs with identical crash signature on clean state",
                "runs_required": 5,
            },
        }
        meta_path.write_text(json.dumps(meta, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
        stats[case["dialect"]] += 1
        print(f"  [+] Created {case['dialect'].upper()}: {case['file']} ({meta['metrics']['statement_count']} stmts, {meta['metrics']['byte_count']} bytes)")

    # Write enhanced cases
    base_map = {c["id"]: c for c in CURATED_CORPUS}
    for enh in ENHANCED_CASES:
        base = base_map[enh["base_id"]]
        sql_path = out_dir / enh["file"]
        meta_path = out_dir / f"{enh['id']}.meta.json"

        sql_content = "\n".join(enh["sql"]) + "\n"
        sql_path.write_text(sql_content, encoding="utf-8")

        meta = {
            "id": enh["id"],
            "file": enh["file"],
            "dialect": enh["dialect"],
            "description": enh["description"],
            "provenance": base["provenance"],
            "target": base["target"],
            "crash_signature": base["crash_signature"],
            "trigger": base["trigger"],
            "metrics": {
                "statement_count": len([s for s in enh["sql"] if s.strip() and not s.strip().startswith("--")]),
                "byte_count": len(sql_content.encode("utf-8")),
            },
            "replay_verification": {
                "requirement": "Must crash consecutively 5 out of 5 runs with identical crash signature on clean state",
                "runs_required": 5,
            },
        }
        meta_path.write_text(json.dumps(meta, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
        stats[enh["dialect"]] += 1
        print(f"  [+] Created Enhanced {enh['dialect'].upper()}: {enh['file']} ({meta['metrics']['statement_count']} stmts, {meta['metrics']['byte_count']} bytes)")

    print(f"\nCorpus generation complete: {sum(stats.values())} testcases across {stats} written to {out_dir}")


def main():
    parser = argparse.ArgumentParser(description="Generate and import authentic DBMS fuzz crash corpus.")
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path("benchmarks/real_fuzz_corpus"),
        help="Target output directory (default: benchmarks/real_fuzz_corpus)",
    )
    args = parser.parse_args()
    write_corpus(args.out_dir)


if __name__ == "__main__":
    main()

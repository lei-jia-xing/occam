# Frontend Grammar Contract

## Scope

`Occam.Frontend.*` is a self-contained, dialect-dispatched SQL frontend over
UTF-8 `ByteString` input. It lexes, splits top-level statements, and builds a
span-annotated AST; it does not execute SQL or model catalog state.

- The grammar target is each engine's **latest stable** documentation at review
  time. There is no version field, flag, or profile: a dialect means the
  current grammar, not a pinned release.
- A `DialectProfile` decides lexical rules, keywords, identifier folding, and
  `DialectFeature` gates. There is no generic fallback grammar; a production
  outside the active dialect is a `ParseUnsupported` error.
- `parseScript` returns the first error and omits client delimiter directives.
  `parseScriptUnits` returns each split unit with its own parse result and span,
  including recognized delimiter directives. Invalid UTF-8 fails the whole
  script. An unterminated lexical construct is reported on the trailing unit.

## Grammar references

One canonical latest-stable entry point per dialect; topic pages are reached
from these indexes.

| Dialect | Official grammar index |
| --- | --- |
| PostgreSQL | <https://www.postgresql.org/docs/current/sql-commands.html> |
| MySQL | <https://dev.mysql.com/doc/refman/en/sql-statements.html> |
| MariaDB | <https://mariadb.com/kb/en/sql-statements/> |
| SQLite | <https://sqlite.org/lang.html> |
| DuckDB | <https://duckdb.org/docs/stable/sql/statements/overview> |
| ClickHouse | <https://clickhouse.com/docs/sql-reference/statements> |

## Module map

| Module | Role |
| --- | --- |
| `Occam.Frontend.Span` | Half-open UTF-8 `ByteSpan`, `Located`, merge/slice, UTF-8 boundary validation. |
| `Occam.Frontend.Lexer` | Single-pass tokenizer; preserves whitespace/comments as trivia and exact source spans. |
| `Occam.Frontend.Splitter` | Splits top-level statements on the active delimiter at nesting depth zero; keeps compound routine/trigger/CASE blocks whole. |
| `Occam.Frontend.AST` | Span-annotated syntax tree; names retain raw spelling and quotedness. |
| `Occam.Frontend.Parser` | Entry points `parseUnit`/`parseScript`; handles trivia stripping, delimiter directives, and statement-end checks. |
| `Occam.Frontend.Parser.Core` | `P` monad, token/name primitives, list combinators, dialect gates, opaque-body scanners. |
| `Occam.Frontend.Parser.Expr` | Pratt expressions, type names, windows, ordering, index keys. |
| `Occam.Frontend.Parser.Query` | WITH/CTE, SELECT, VALUES/TABLE/FROM-first bodies, FROM/JOIN, query tail. |
| `Occam.Frontend.Parser.Stmt` | Statement dispatch, INSERT/UPDATE/DELETE/MERGE, session/utility statements. |
| `Occam.Frontend.Parser.DDL` | CREATE/DROP/ALTER and column/table definitions. |
| `Occam.Frontend.Context` | Per-statement analysis context, identifier normalization, and source slicing. |
| `Occam.Frontend.Semantics.*` | Pure-Haskell semantic fact extraction (catalog ops, session/tx effects, barriers). |
| `Occam.Frontend.Edits.*` | Exact-span intra-statement edit generation with typed priority tiers (`EditTier`). |
| `Occam.Frontend.Columns.*` | Single-point `EditColumn` extraction and atomic cross-statement `ColumnTarget` assembly. |
| `Occam.Frontend.Pipeline` | Top-level pure-Haskell `parseFrontend` pipeline invoked directly by `Occam.Parser`. |

## Span and trivia contract

- Offsets are 0-based byte offsets into the original UTF-8 input; spans are
  half-open `[start, end)`. `Span` validation rejects out-of-range and
  mid-codepoint boundaries.
- The lexer keeps whitespace and comments as `TokTrivia`; the splitter slices
  the token vector without dropping span positions; parsers receive only
  substantive tokens.
- A statement span starts at its first substantive token and ends at its last
  substantive token. Leading/trailing trivia and the delimiter are excluded.
  Comments **between** statement tokens are inside the span.
- Expression/node spans cover the full node text, so comments between operands
  are included (for example `a /* c */ + b`).
- At end of input, a missing token produces a zero-width span at the unit end.
- Opaque bodies (`OpaqueBody`) record the consumed byte range; their internals
  are not parsed.
- Names keep `namePartRaw` (quotes included when quoted) and
  `namePartQuoted`; case folding is a consumer decision.

## Dialect dispatch

`Occam.Parser.Dialect` provides the lexical profile and feature set. Parser
code asserts them with `requireDialectP` or `requireFeatureP`; both report
`ParseUnsupported` with the offending construct and dialect name.

| Feature | PostgreSQL | MySQL | MariaDB | SQLite | DuckDB | ClickHouse |
| --- | :-: | :-: | :-: | :-: | :-: | :-: |
| `ON CONFLICT` / upsert | yes | no | no | yes | yes | no |
| `ON DUPLICATE KEY UPDATE` | no | yes | yes | no | no | no |
| `RETURNING` | yes | no | yes | yes | yes | no |
| `MERGE` | yes | no | no | no | yes | no |
| `COPY` | yes | no | no | no | yes | no |
| `PRAGMA` | no | no | no | yes | yes | no |
| `ATTACH`/`DETACH` | no | no | no | yes | yes | yes |
| prepared statements | yes | yes | yes | no | yes | no |
| sequence DDL | yes | no | yes | no | yes | no |
| `CREATE MATERIALIZED VIEW` | yes | no | no | no | no | yes |
| `DROP MATERIALIZED VIEW` | yes | no | no | no | no | no |
| virtual tables | no | no | no | yes | no | no |
| `CREATE MACRO` | no | no | no | no | yes | no |
| `PIVOT`/`UNPIVOT` | no | no | no | no | yes | no |
| `ENGINE` / `SETTINGS` / `PREWHERE` | no | no | no | no | no | yes |
| `QUALIFY` | no | no | no | no | yes | yes |
| full-text index / index prefix | no | yes | yes | no | no | no |
| `::` cast | yes | no | no | no | yes | yes |
| `.:` typed path cast | no | no | no | no | no | yes |

## Per-dialect coverage

### PostgreSQL

- Queries: WITH/CTE (query or DML body), SELECT list, `SELECT INTO` table form,
  `DISTINCT ON`, FROM/joins (including LATERAL), `TABLESAMPLE`,
  GROUPING SETS/ROLLUP/CUBE, `FOR UPDATE/SHARE/NO KEY UPDATE/KEY SHARE`,
  `LIMIT/OFFSET/FETCH`, set operations.
- DML: INSERT with `ON CONFLICT`, UPDATE ... FROM, DELETE ... USING, MERGE,
  RETURNING.
- DDL: CREATE/DROP/ALTER TABLE, INDEX (name optional, `INCLUDE`, partial
  `WHERE`), VIEW, SEQUENCE, SCHEMA, DATABASE, STATISTICS, FUNCTION/PROCEDURE,
  TRIGGER, RULE, POLICY, partitions, RLS.
- Utility: COPY, ANALYZE, VACUUM, SET, transactions/savepoints, PREPARE,
  EXECUTE, DEALLOCATE, NOTIFY, TRUNCATE, EXPLAIN.

### MySQL

- Queries: WITH/CTE, SELECT (including `INTO @user_var`), `TABLE t`, `VALUES ROW(...)`, joins, LATERAL,
  `LIMIT offset,count`, DISTINCT, GROUP BY/HAVING, window/OVER; `QUALIFY` is
  rejected (MySQL 8.4 only enables `QUALIFY` with the experimental
  `hypergraph_optimizer`).
- DML: INSERT/REPLACE with `ON DUPLICATE KEY UPDATE` (including
  `INSERT ... VALUES ROW(...)`), multi-table UPDATE/DELETE (`ORDER BY` /
  `LIMIT` / `RETURNING` rejected on multi-table `DELETE`), `RETURNING` is
  rejected.
- DDL: CREATE/DROP/ALTER TABLE (`DROP TEMPORARY TABLE`), INDEX (including FULLTEXT, prefix lengths, and
  double-parenthesized functional key parts `((expr))`), VIEW (`ALGORITHM`),
  DATABASE, TRIGGER, FUNCTION/PROCEDURE, table options (`ENGINE`, `DEFAULT
  CHARSET`, `COLLATE`, `AUTO_INCREMENT`, ...).
- Utility: SET (including `SET NAMES`, `CHARACTER SET`, scoped values),
  transaction control, PREPARE/EXECUTE/DEALLOCATE, ANALYZE, OPTIMIZE, RENAME
  TABLE, SHOW, TRUNCATE, EXPLAIN; `RESET` is rejected.

### MariaDB

- Same statement families as MySQL, with MariaDB extensions: `TABLE t`
  queries, `RETURNING` after INSERT/REPLACE/UPDATE/DELETE (single-table),
  `CREATE [OR REPLACE] SEQUENCE`/`INDEX`/`DATABASE`/`SCHEMA`, `DROP TEMPORARY TABLE/SEQUENCE`,
  `PREPARE ... FROM` expressions, `USING HASH` index methods, expression index keys `(f(col))`,
  and `ALTER TABLE ... ALGORITHM=...`. `LATERAL` is not accepted; `WITHIN GROUP
  (ORDER BY ...)` requires an `OVER (...)` window specification.
- `ON DUPLICATE KEY UPDATE`, `FULLTEXT`, and index prefix lengths apply as in
  MySQL.

### SQLite

- Queries: WITH/CTE (`MATERIALIZED`/`NOT MATERIALIZED`), SELECT, `VALUES`,
  joins (including RIGHT/FULL), LIMIT `offset,count`, window functions,
  aggregate `ORDER BY` and `FILTER`, `IN ()`.
- DML: INSERT/REPLACE, UPSERT (`ON CONFLICT`), UPDATE ... FROM, DELETE,
  RETURNING.
- DDL: CREATE/DROP TABLE (`STRICT`, `WITHOUT ROWID`), INDEX (partial),
  VIEW, VIRTUAL TABLE, TRIGGER (body opaque), ALTER TABLE ADD/DROP/RENAME
  COLUMN and RENAME TO.
- Utility: PRAGMA, ATTACH/DETACH, VACUUM (including `INTO`), ANALYZE,
  transactions/savepoints, EXPLAIN.

### DuckDB

- Queries: WITH/CTE, SELECT, `TABLE`, `VALUES`, `FROM`-first queries with optional
  projection, filtering, grouping, and window clauses, LATERAL,
  ASOF joins (left/right), `TABLESAMPLE`, `DISTINCT ON`, `QUALIFY`, window
  functions, set operations, PIVOT/UNPIVOT.
- DML: INSERT, UPDATE ... FROM, DELETE ... USING, MERGE, RETURNING.
- DDL: CREATE/DROP/ALTER TABLE, INDEX, VIEW, SEQUENCE, SCHEMA, MACRO
  (`CREATE MACRO ... AS TABLE`), `CREATE OR REPLACE` table/view/macro.
- Utility: ATTACH/DETACH (including `IF NOT EXISTS`), COPY, ANALYZE, VACUUM,
  SET, PRAGMA, PREPARE/EXECUTE/DEALLOCATE, EXPLAIN.

### ClickHouse

- Queries: SELECT with `PREWHERE`, `QUALIFY`, `SETTINGS`, `FORMAT`, `FINAL`,
  `SAMPLE ... [OFFSET ...]`, `FROM`-first queries (`FROM t SELECT ...`),
  `ARRAY JOIN`/`LEFT ARRAY JOIN`, `GLOBAL` and strictness join modifiers,
  ASOF joins (`ASOF LEFT JOIN` and `LEFT ASOF JOIN`; `ASOF RIGHT JOIN` is
  rejected to match ClickHouse execution), `DISTINCT ON`,
  `GROUPING SETS`/`ROLLUP`/`CUBE`, trailing SELECT-list commas, window
  functions, `FILTER (WHERE ...)`, lambda expressions, `.:` typed path casts,
  array subscripts (`LATERAL` subqueries are rejected).
- DML: INSERT, UPDATE/DELETE via `ALTER TABLE ... UPDATE/DELETE` (lightweight
  mutations), no `RETURNING`.
- DDL: CREATE/DROP/ALTER TABLE (including `ENGINE`, `SETTINGS`, projections,
  skipping indexes, partitions, trailing `ALTER TABLE ... SETTINGS`),
  DROP `TEMPORARY TABLE`/`VIEW`, CREATE `[TEMP|OR REPLACE]` VIEW, CREATE
  `[OR REPLACE]` MATERIALIZED VIEW (`TO`, `ENGINE`, `SETTINGS`, `POPULATE`;
  `CREATE TEMPORARY MATERIALIZED VIEW` and `DROP MATERIALIZED VIEW` are
  rejected), CREATE `[OR REPLACE]` FUNCTION, DATABASE, sequences are not
  supported, RENAME TABLE.
- Utility: ATTACH/DETACH objects, OPTIMIZE TABLE, SHOW, TRUNCATE, USE, SET,
  EXPLAIN, `SYNC` drop/detach modifiers.

## Known unsupported and over-accepted forms

Unsupported forms fail with `ParseUnsupported` or `ParseUnexpectedToken`; they
are not silently re-emitted as opaque bodies unless listed as opaque.

- Cross-dialect: `CREATE TYPE/DOMAIN/EXTENSION/AGGREGATE/OPERATOR/COLLATION/
  CONVERSION/ROLE/USER/TABLESPACE/PUBLICATION/SUBSCRIPTION/SERVER/FOREIGN
  TABLE/EVENT`, `GRANT`/`REVOKE`, `COMMENT`, `CALL`/`DO`, `REFRESH
  MATERIALIZED VIEW`, `CREATE INDEX CONCURRENTLY`/`IF NOT EXISTS`, and
  multi-action `ALTER TABLE` forms beyond the `AlterAction` constructors.
- PostgreSQL: `SELECT INTO` variants other than the table form, `VACUUM
  (options)`, `ALTER TABLE ... SET STATISTICS/SET (...)/USING/OWNER/SET
  SCHEMA`, `ALTER TABLE ... VALIDATE CONSTRAINT`, `CREATE TABLE ... LIKE`
  outside the parenthesized element list, and `DROP TABLE`/`DROP INDEX`
  option combinations outside the AST fields.
- MySQL/MariaDB: `INSERT ... SET`, `INSERT IGNORE`, `LOAD DATA`, `LOCK/UNLOCK
  TABLES`, `HANDLER`, `CREATE/DROP EVENT`, `CREATE/DROP SPATIAL`, `CHECK
  TABLE`, and full `SHOW` grammar. `RETURNING` is rejected for MySQL.
- SQLite: trigger bodies are opaque; `ALTER TABLE` is limited to ADD/DROP/
  RENAME COLUMN and RENAME TO; complex `CREATE VIRTUAL TABLE` module arguments
  may degrade to expression/column nodes; there is no `DELETE ... USING`.
- DuckDB: `CREATE SECRET`, `CREATE TYPE`, external-resource DDL, `ATTACH`
  options and `ATTACH OR REPLACE`, `SUMMARIZE`/`DESCRIBE`, and extension DDL.
  `DETACH ... IF EXISTS` is accepted but not independently verified against
  an engine grammar page.
- ClickHouse: `CREATE DICTIONARY`, `SYSTEM`, `KILL`, `EXISTS`, `WATCH`,
  `EXCHANGE`, `ON CLUSTER` clauses, `CREATE TABLE ... AS table_function`,
  `ALTER TABLE ... FREEZE/UNFREEZE` and other actions outside `AlterAction`.
- Over-accepted in the current grammar:
  - `NULLS FIRST`/`LAST`, `OVER`, and bracket array literals still parse broadly
    in expression position (though `FILTER`/`OVER`/`WITHIN GROUP` are now rejected on
    PostgreSQL/DuckDB special value expressions such as `COALESCE` and `NULLIF`).
  - `NATURAL JOIN ... ON/USING` is now rejected across all dialects, `CROSS JOIN ...
    ON/USING` is rejected in PostgreSQL/DuckDB/ClickHouse, `FOR UPDATE`/`FOR SHARE`/
    `FOR NO KEY UPDATE`/`FOR KEY SHARE` are dialect-gated, `WITHIN GROUP` is gated to
    PostgreSQL, DuckDB, and (with `OVER`) MariaDB, `FILTER` is gated to PostgreSQL/SQLite/DuckDB/ClickHouse,
    `CREATE [TEMP|TEMPORARY|UNLOGGED]` is validated per object type and dialect, and
    PostgreSQL `ALTER TABLE RENAME` is rejected when combined with other actions.
  - Remaining broader productions: multi-name `DROP TABLE`, SQLite `REFERENCES schema.t`
    and `CREATE TABLE (cols) AS SELECT`, and some `ALTER TABLE` action combinations are
    broader than individual engines' own grammars.
- Lexical over-acceptance: MySQL/MariaDB `--` starts a line comment even without
  the following whitespace/control byte their lexers require; `N'...'`,
  `B'...'`, and `X'...'` literals are lexed for every dialect; and
  `U&"..."`/`U&'...'` PostgreSQL Unicode escapes are not recognized as a unit.

## Seed conformance corpus

`test/seeds/` is the shared, reviewable conformance corpus for the six target
engines. It is independent of the Haskell test loader: each file is plain UTF-8
SQL whose blocks can also be fed directly to the real engine CLIs.

| File | Cases | Units | Status |
| --- | ---: | ---: | --- |
| `test/seeds/postgresql.sql` | 17 | 41 | representative branches + shared cases |
| `test/seeds/mysql.sql` | 9 | 20 | representative branches + shared cases |
| `test/seeds/mariadb.sql` | 10 | 21 | representative branches + shared cases |
| `test/seeds/sqlite.sql` | 13 | 32 | representative branches + shared cases |
| `test/seeds/duckdb.sql` | 12 | 29 | representative branches + shared cases |
| `test/seeds/clickhouse.sql` | 13 | 25 | representative branches + shared cases |
| `test/seeds/invalid.sql` | 16 | 16 | curated parser rejects |
| **Total** | **90** | **184** | **FULL — validated live against all six engines** |

### Block format

- A block starts at column zero with `-- @case: <globally unique id>`,
  immediately followed by `-- @dialects:` and `-- @units:`.
- `@dialects` is an exhaustive *accept* set: the same bytes must be accepted by
  every listed canonical engine. It is not a file-owner annotation. `invalid.sql`
  is the exception: its `@dialects` is the set expected to reject, while other
  profiles must not be assumed to reject.
- `@units` lists the splitter's expected units in order. Entries are
  `sql=<delimiter>` (for example `sql=;`, `sql=$$`, `sql=//`), bare `sql` for a
  final unit with no terminator, or `directive=<argument>` for a client
  `DELIMITER` directive (including `directive=;`).
- Exactly `N - 1` literal `-- @unit` lines mark inter-unit boundaries. They stay
  in the SQL body as ordinary comments; only `-- @case` starts a new block.
- Cases are self-contained and use unique object names. Setup statements are
  included only when the probed production needs them. Cross-block dependencies,
  external IO, network access, and databases outside the fixture are forbidden.
- Ordinary engine commands receive the original SQL bytes after the three
  metadata header lines, including `-- @unit` comments. They never receive a
  split or rewritten script.

### Coverage ledger

PostgreSQL (`libpg_query` 18.0.0), DuckDB (`libduckdb` 1.5.5), and SQLite
(`direct-sqlite`) run in-process inside `occam-test`. When run inside
`nix develop .#syntax` (or `make syntax-check`), ClickHouse (`26.8.7.19-lts`
via `clickhouse format --multiquery`), MySQL (`8.4.11` `mysqld` via
`dlopen libmariadb.so.3` and `mysql` CLI), and MariaDB (`11.4.13` `mariadbd`
via `dlopen libmariadb.so.3` and `mariadb` CLI) also run live against seeds,
fuzz corpus cases, regression cases, and differential fuzz properties with zero
Dockerfiles and zero test-oracle linkage in production binaries.

| Dialect | Representative branches covered | Explicitly not yet covered |
| --- | --- | --- |
| PostgreSQL | identity/generated/`UNLOGGED` table DDL; `INCLUDE` + partial index; `CREATE STATISTICS`; materialized views + `REFRESH`; range partitions; RLS/`POLICY`/`RULE`; `ON CONFLICT (key)` & `ON CONSTRAINT`; shared `MERGE`; `COPY`; `PREPARE`/`EXECUTE`/`DEALLOCATE`; transaction modes/savepoints; `FILTER`, `WITHIN GROUP`, `FOR NO KEY UPDATE SKIP LOCKED`, `DISTINCT ON`; dollar/E/UTF-8 lexical; sequence/view/rename/CTE/`RETURNING`/`LIMIT` | broader `ALTER TABLE` action combinations; `CREATE DOMAIN`/`TYPE`/`OPERATOR`/`CONVERSION` edge options |
| MySQL | table options, prefix/fulltext keys, MySQL 8.0 collation; shared fulltext/hash index and `ALTER ... ALGORITHM`; before-insert trigger; `ON DUPLICATE KEY UPDATE`; multi-table `UPDATE`; `VALUES ROW`; `DELIMITER $$` procedure, `//` function, and glued `AS a$$` tail; UTF-8 introducer/cast; shared view, rename, recursive CTE, comma/offset limits | view algorithms and standalone `ALTER` DDL; multi-table `DELETE`; utility `PREPARE`/`SET`/`EXPLAIN`/`ANALYZE`/`OPTIMIZE`/`SHOW`; JSON query forms; window aggregates; `INSERT ... RETURNING` is a curated reject only |
| MariaDB | shared sequence with PostgreSQL/DuckDB; shared fulltext/hash index with MySQL; shared `ALTER ... ALGORITHM`; `INSERT ... RETURNING` shared with PostgreSQL/SQLite/DuckDB; `WITHIN GROUP (ORDER BY ...) OVER ()`; `PREPARE ... FROM` expression; `//` routine and glued `$$` tail; UTF-8 introducer/cast; shared view/rename/recursive CTE/limits | `UPDATE`/`DELETE ... RETURNING`; `TABLE` query; virtual-column `ALTER`; utility `SET`/`ANALYZE`/`OPTIMIZE`; JSON/fulltext query forms |
| SQLite | `STRICT` table; shared partial index; `ALTER ADD`/`DROP`/`RENAME COLUMN`; upsert + `RETURNING`; opaque trigger body; `PRAGMA`/`ATTACH`/`DETACH`/`VACUUM`; `BEGIN IMMEDIATE` + savepoints; window aggregates + `FILTER`; recursive CTE; comma vs `OFFSET` limits; bracket vs double-quote + UTF-8/blob lexical; shared view | virtual tables/FTS (deliberately excluded until optional build features are verified); `RIGHT`/`FULL` joins |
| DuckDB | `CREATE MACRO`, `CREATE OR REPLACE` table/macro; shared view; `INSERT`/`DELETE`/`UPDATE ... FROM ... RETURNING`; `FROM`-first query; `PIVOT`; `QUALIFY`; `ASOF RIGHT JOIN`; `ATTACH`/`DETACH`/`PRAGMA`/`PREPARE`/`EXECUTE`/`DEALLOCATE`; `COPY`; `GROUPING SETS`; nested block comments, dollar/E lexical, sequence/merge/upsert/rename/CTE/limits | nested `STRUCT`/`LIST` probes (removed as ambiguous for the initial corpus); temporal/ASOF edge variants |
| ClickHouse | MergeTree with projection/TTL/settings; `CREATE [OR REPLACE] FUNCTION` lambda; materialized view `TO`; `INSERT SELECT`; `ALTER ... UPDATE`/`DELETE`; `OPTIMIZE ... FINAL`; `FROM`-first query; `PREWHERE`/`ARRAY JOIN`/`SETTINGS`/`FORMAT`; `FILTER`; `.:` typed path cast; `arrayMap` lambda; `GLOBAL`/`ANY`/`ANTI`/`SEMI`/`ASOF` joins; `GROUPING SETS` and trailing select-list comma; `DETACH`/`ATTACH`; UTF-8/cast/bare select; `DISTINCT ON`, `QUALIFY`, view | JSON/Dynamic options; `FORMAT` variants beyond `Null`; `ON CLUSTER` |
| Curated rejects | `DELIMITER;`; malformed `ALTER ... DROP KEY`; `BEFORE SELECT` trigger; missing routine body; `BEGIN FOOBAR`; ClickHouse `DROP MATERIALIZED VIEW`; SQLite `DELETE ... USING`; MySQL `INSERT ... RETURNING`; DuckDB `SAVEPOINT`; `NATURAL JOIN ... ON/USING`; non-locking `FOR UPDATE`; `CREATE TEMP POLICY`; PG multi-action `ALTER RENAME`; SQLite/MySQL and bare MariaDB `WITHIN GROUP`; `CROSS JOIN ... ON` | None — all six target engines are validated live via `occam-test` in `.#syntax` |

## Verification

- `test/PgQueryOracle.hs` + `test/cbits/pg_query_shim.c` provide a test-only
  `dlopen` wrapper around `libpg_query` (PostgreSQL 18.0.0) without linking
  any C parser into `lib:occam` or `exe:occam`.
- `test/ExtraEngineOracles.hs` provides test-only syntax oracles for
  ClickHouse `26.8.7.19-lts` (`clickhouse format --multiquery`), MySQL
  `8.4.11`, and MariaDB `11.4.13` (unix-socket `mysqld`/`mariadbd` daemons
  queried via `dlopen libmariadb.so.3` plus client CLI for `DELIMITER` scripts),
  without Dockerfiles or C oracle linkage in production binaries.
- `test/FrontendChecks.hs` covers span/UTF-8 boundaries, lexer dialect
  isolation, splitter boundaries/DELIMITER, and oracle-proof parity.
- `test/FrontendSeedChecks.hs` runs the 6-dialect seed matrix (`90` cases /
  `184` units) plus bidirectional positive/invalid/cross-dialect alignment across
  all six live engines (`libpg_query`, `libduckdb`, `direct-sqlite`, plus
  ClickHouse 26.8, MySQL 8.4, and MariaDB 11.4 when `.#syntax` env vars are set).
- `test/SyntaxAlignmentChecks.hs` checks `benchmarks/real_fuzz_corpus/*.sql`
  across all six dialects (`71` cases total), `test/seeds/regression_cases.sql`
  (`PostgreSQL`, `DuckDB`, `ClickHouse`, `MySQL`, `MariaDB`), and 36
  QuickCheck differential syntax fuzzing properties across all six engines
  (100 tests each for PG/DuckDB/SQLite/MySQL/MariaDB, 25 tests each for
  ClickHouse).
- `test/ParserChecks.hs` covers exact spans, precedence, error spans, dialect
  accept/reject, representative AST shapes, scripts, and all 71
  `benchmarks/real_fuzz_corpus/*.sql` files.
- `test/FrontendFacadeChecks.hs` checks the product parser's Haskell facts,
  dependency slices, and absolute clause edit ranges.
- `test/FrontendCorpusChecks.hs` runs pure-Haskell corpus regression checks
  across all 71 `benchmarks/real_fuzz_corpus/*.sql` cases, `benchmarks/corpus/*.sql`,
  and the 528 regression SQL cases in `test/seeds/regression_cases.sql`.
- `cabal test occam-fuzz` runs generated reduction properties for all six
  dialects. The properties check statement counts, syntax, predicate
  preservation, size monotonicity, distractor removal, and fixed points.
- Run `cabal build all --enable-tests --ghc-options=-Werror` and
  `make syntax-check` (or `nix develop .#syntax --command cabal test occam-test --ghc-options=-Werror`)
  for the full six-engine verification suite.

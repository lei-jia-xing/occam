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
  `LIMIT offset,count`, DISTINCT, GROUP BY/HAVING, window/OVER, `QUALIFY` is
  not accepted (not in the engine grammar).
- DML: INSERT/REPLACE with `ON DUPLICATE KEY UPDATE` (including
  `INSERT ... VALUES ROW(...)`), multi-table UPDATE/DELETE, `RETURNING` is
  rejected.
- DDL: CREATE/DROP/ALTER TABLE, INDEX (including FULLTEXT and prefix lengths),
  VIEW (`ALGORITHM`), DATABASE, TRIGGER, FUNCTION/PROCEDURE, table options
  (`ENGINE`, `DEFAULT CHARSET`, `COLLATE`, `AUTO_INCREMENT`, ...).
- Utility: SET (including `SET NAMES`, `CHARACTER SET`, scoped values),
  transaction control, PREPARE/EXECUTE/DEALLOCATE, ANALYZE, OPTIMIZE, RENAME
  TABLE, SHOW, TRUNCATE, EXPLAIN.

### MariaDB

- Same statement families as MySQL, with MariaDB extensions: `TABLE t`
  queries, `RETURNING` after INSERT/REPLACE/UPDATE/DELETE, `CREATE SEQUENCE`,
  `PREPARE ... FROM` expressions, `USING HASH` index methods, and
  `ALTER TABLE ... ALGORITHM=...`. `LATERAL` is not accepted.
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
  `SAMPLE ... [OFFSET ...]`, `ARRAY JOIN`/`LEFT ARRAY JOIN`, `GLOBAL` and
  strictness join modifiers, ASOF joins, `DISTINCT ON`, `LATERAL` subqueries,
  `GROUPING SETS`/`ROLLUP`/`CUBE`, trailing SELECT-list commas, window
  functions, lambda expressions, `.:` typed path casts, array subscripts.
- DML: INSERT, UPDATE/DELETE via `ALTER TABLE ... UPDATE/DELETE` (lightweight
  mutations), no `RETURNING`.
- DDL: CREATE/DROP/ALTER TABLE (including `ENGINE`, `SETTINGS`, projections,
  skipping indexes, partitions), CREATE VIEW/MATERIALIZED VIEW (`TO`,
  `ENGINE`, `SETTINGS`, `POPULATE`), CREATE FUNCTION, DATABASE, sequences are
  not supported, RENAME TABLE.
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
- Over-accepted in the current grammar: `NATURAL JOIN` may carry `ON`/`USING`;
  `NULLS FIRST/LAST`, `FILTER`, `OVER`, and bracket array literals are accepted
  wherever the expression parser sees them; `TEMP TABLE` is not feature-gated
  per engine; `FOR SHARE`/`NO KEY UPDATE`/`KEY SHARE` are accepted by
  MySQL/MariaDB alongside PostgreSQL; multi-name `DROP TABLE` and some
  `ALTER TABLE` action combinations (for example `DROP PRIMARY KEY` or
  `ADD CONSTRAINT`) are broader than some engines, and `ALTER TABLE` actions
  are not gated as tightly as the engines' own grammars.  These are
  grammar-tightening work, not execution semantics.
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
| `test/seeds/postgresql.sql` | 9 | 15 | representative branches + shared cases |
| `test/seeds/mysql.sql` | 9 | 20 | representative branches + shared cases |
| `test/seeds/mariadb.sql` | 9 | 19 | representative branches + shared cases |
| `test/seeds/sqlite.sql` | 9 | 15 | representative branches + shared cases |
| `test/seeds/duckdb.sql` | 8 | 15 | representative branches + shared cases |
| `test/seeds/clickhouse.sql` | 9 | 16 | representative branches + shared cases |
| `test/seeds/invalid.sql` | 9 | 9 | curated parser rejects |
| **Total** | **62** | **109** | **representative, not FULL grammar** |

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

The status is **representative branch coverage, not FULL grammar**. A listed
branch is covered only for the exact variant in the seed; sibling branches
remain backlog until they have their own focused case. `FULL` additionally requires independent validation against each engine, the
Phase 4 product facade cutover, and semantic-facade tests.

| Dialect | Representative branches covered | Explicitly not yet covered |
| --- | --- | --- |
| PostgreSQL | identity/generated/default table DDL; `INCLUDE` + partial index; `ON CONFLICT (key)` shared with SQLite/DuckDB; `ON CONFLICT ON CONSTRAINT`; shared `MERGE`; shared `DISTINCT ON`; dollar-quoted function + trigger; dollar/E/UTF-8 lexical; shared sequence, view, partial index, column rename, recursive CTE, `LIMIT ... OFFSET`, insert/delete `RETURNING`, bare `SELECT` | grouped/window aggregates and `FILTER`; `CREATE STATISTICS`; materialized views; `COPY`; transaction modes/savepoints; RLS/policy/rule; broader `ALTER TABLE` actions; `PREPARE`/`EXECUTE`; partition DDL |
| MySQL | table options, prefix/fulltext keys, MySQL 8.0 collation; shared fulltext/hash index and `ALTER ... ALGORITHM`; before-insert trigger; `ON DUPLICATE KEY UPDATE`; multi-table `UPDATE`; `VALUES ROW`; `DELIMITER $$` procedure, `//` function, and glued `AS a$$` tail; UTF-8 introducer/cast; shared view, rename, recursive CTE, comma/offset limits | view algorithms and standalone `ALTER` DDL; multi-table `DELETE`; utility `PREPARE`/`SET`/`EXPLAIN`/`ANALYZE`/`OPTIMIZE`/`SHOW`; JSON query forms; window aggregates; `INSERT ... RETURNING` is a curated reject only |
| MariaDB | shared sequence with PostgreSQL/DuckDB; shared fulltext/hash index with MySQL; shared `ALTER ... ALGORITHM`; `INSERT ... RETURNING` shared with PostgreSQL/SQLite/DuckDB; `PREPARE ... FROM` expression; `//` routine and glued `$$` tail; UTF-8 introducer/cast; shared view/rename/recursive CTE/limits | `UPDATE`/`DELETE ... RETURNING`; `TABLE` query; virtual-column `ALTER`; utility `SET`/`ANALYZE`/`OPTIMIZE`; JSON/fulltext query forms |
| SQLite | `STRICT` table; shared partial index with PostgreSQL; upsert with preceding insert + conflict update + `RETURNING` shared with PostgreSQL/DuckDB; opaque trigger body; shared column rename; shared recursive CTE; comma vs `OFFSET` limits (comma branch with MySQL/MariaDB); bracket vs double-quote identifiers plus UTF-8/cast/blob lexical; shared view | virtual tables/FTS (deliberately excluded until optional build features are verified); `ATTACH`/`PRAGMA`/`VACUUM`; `ALTER ADD`/`DROP COLUMN`; right/full joins; savepoints; window aggregates; aggregate `FILTER` |
| DuckDB | `CREATE MACRO`; shared view; insert `RETURNING` shared with PostgreSQL/MariaDB/SQLite; `DELETE ... USING ... RETURNING` shared with PostgreSQL; `FROM`-first query; `PIVOT`; `QUALIFY` shared with ClickHouse; `ASOF LEFT JOIN`; shared sequence/distinct-on/merge/upsert/rename/recursive-CTE/limits; dollar/E lexical shared with PostgreSQL | nested `STRUCT`/`LIST` probes (removed as ambiguous for the initial corpus); `COPY`; `PREPARE`/`ATTACH`/`PRAGMA`; `UPDATE ... FROM`; grouping sets; `CREATE OR REPLACE`; temporal/ASOF edge variants |
| ClickHouse | MergeTree with projection/TTL/settings; `CREATE FUNCTION` lambda; materialized view `TO`; `INSERT SELECT`; `ALTER ... UPDATE`; `PREWHERE`/`ARRAY JOIN`/`SETTINGS`/`FORMAT`; `GROUPING SETS` and trailing select-list comma; `DETACH`/`ATTACH`; UTF-8/cast/bare select; shared `DISTINCT ON`, `QUALIFY`, and view | `.:` typed path cast; `ALTER ... DELETE`; `OPTIMIZE`; JSON/Dynamic; array-map lambdas; global/strictness joins; `FORMAT` variants beyond `Null`; `ON CLUSTER` |
| Curated rejects | `DELIMITER;`; malformed `ALTER TABLE ... DROP KEY PRIMARY KEY`; `BEFORE SELECT` trigger; missing routine body; `BEGIN FOOBAR`; ClickHouse `DROP MATERIALIZED VIEW`; SQLite `DELETE ... USING`; MySQL `INSERT ... RETURNING`; DuckDB `SAVEPOINT` | Independent classification of engine rejections |

## Verification

- `test/FrontendChecks.hs` covers span/UTF-8 boundaries, lexer dialect
  isolation, splitter boundaries/DELIMITER, and oracle-proof parity.
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
  `cabal test occam-test` for the seed corpus, embedded SQLite and DuckDB
  checks, and the rest of the integration suite.

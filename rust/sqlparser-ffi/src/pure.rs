//! Per-dialect pure-builtin function tables.
//!
//! This lifts the blanket "ANY function call ⇒ global opaque barrier" rule for
//! small allowlists of provably-pure builtins. "Pure" here means ONLY: no
//! hidden writes, no sequence use, no volatile/session effects. It does NOT
//! mean "no reads": reads flow through function arguments and are extracted by
//! the existing expression analysis independently of fencing, so lifting the
//! fence never drops a read edge.
//!
//! Fencing stays conservative everywhere else:
//! - Unknown functions fence (each allowlist is closed; anything not listed is
//!   unmodeled and keeps today's barrier verbatim).
//! - Volatile and sequence-adjacent names (`random`, `nextval`/`currval`/
//!   `setval`/`lastval`, clock calls such as `now`/`current_timestamp`,
//!   `version`, aggregate state functions) are deliberately absent and fence.
//! - Window/filter/grouping machinery (`OVER`, `FILTER`, `WITHIN GROUP`,
//!   null treatment, parametric calls) fences even under a builtin name.
//! - Qualification fences: only unqualified single-part names lift, so no
//!   schema resolution is ever assumed (`myschema.abs` may name a user
//!   routine; even `pg_catalog.abs` fences to keep the rule minimal).
//! - Every dialect outside SQLite, PostgreSQL, MySQL, and the Generic subset
//!   below keeps the blanket fence bit-identically.
//!
//! Aggregate allowlist ([`AGGREGATE_BUILTINS`], same three dialects): `avg`,
//! `count`, `max`, `min`, and `sum` lift in the plain call shape only (no
//! `OVER`/`FILTER`/`WITHIN GROUP`, shared gate with the scalar table). The
//! proof: each name is a core aggregate on all three engines computing a
//! closed value over its input rows — no writes, no sequence use, no
//! volatility. Argument shapes (`DISTINCT`, `ORDER BY`, `*`, expressions,
//! bare subqueries) select or order the input values, so they affect values,
//! not effects, and their reads are extracted independently. Grouping
//! machinery (`GROUP BY`/`HAVING`) lives in sibling clauses visited
//! independently; aggregates add no hidden reads beyond their arguments.
//! None of the five takes seed or parametric arguments (the parameters gate
//! fences parametric calls anyway). Session interaction is value- or
//! acceptance-only and explicitly out of scope per the effects-not-values
//! doctrine: `ONLY_FULL_GROUP_BY` governs whether a query is accepted, never
//! what an accepted query does; `div_precision_increment` and
//! `group_concat_max_len`-style settings alter result values (precision,
//! truncation), not effects — and `group_concat` itself is not listed.
//! Errors (overflow, division by zero) are errors, not writes. Everything
//! else aggregate-adjacent stays fenced: `OVER` (window `ROWS` framing with
//! `ORDER BY` ties selects nondeterministically among peers, which is
//! physical-order-dependent and volatile-like), `FILTER`/`WITHIN GROUP`
//! (deliberately outside the proven set so one shared shape gate covers both
//! tables), statistical siblings (`stddev`/`variance`/...) and
//! ordered-set/inverse-distribution functions (not uniform core builtins —
//! e.g. SQLite has neither `stddev` nor `percentile_cont` — so no uniform
//! entry is sound; on an engine without the builtin the name would resolve
//! to an extension or application-defined routine with unknown effects).
//!
//! Generic subset ([`GENERIC_PURE_BUILTINS`]): `GenericSQL` means an unknown
//! engine, so a lift must hold on ANY engine. Only `coalesce` and `nullif`
//! qualify: they are SQL-standard reserved-keyword special forms, so they
//! cannot name user routines on any conforming engine (reserved keywords are
//! not definable as function names), and their semantics — first-non-NULL
//! selection, equality-conditional NULL — admit no writes, sequences, or
//! volatility by definition. Every other name stays fenced on Generic:
//! ordinary names (`abs`, `sum`, ...) might not exist as builtins on some
//! engine and could resolve to user or extension routines with unknown
//! effects (custom aggregates especially are built from arbitrary
//! functions). Only the unquoted keyword spelling lifts: quotes make the
//! name an identifier, which on an unknown engine may name a user routine.
//! Matching is case-insensitive because keywords are case-insensitive on
//! every engine.
//!
//! The tables are consulted in both fence positions: evaluated statements (via
//! [`crate::edits::SequenceVisitor`], covering `SELECT` projections, DML, and
//! other immediately-executed expressions) and stored/deferred expressions
//! (via `has_function`, covering column `DEFAULT`s, `CHECK` constraints, view
//! definitions, index expressions, and trigger `WHEN` clauses). A stored pure
//! expression's later execution is effect-free, so lifting it is consistent
//! with non-function stored expressions, which never fenced. Stored volatile
//! calls (e.g. `DEFAULT CURRENT_TIMESTAMP`) still fence through the allowlist.
//!
//! User shadowing: a script may `CREATE FUNCTION abs(...)` redefining a
//! builtin name. The rule is a coarse script-level disable: ANY routine DDL
//! ([`is_routine_ddl`]) or ANY parse error anywhere in the script disables the
//! tables for every unit, restoring today's blanket fence. This is sound and
//! minimal because (a) routine-DDL units independently emit global
//! `UnknownStatement` barriers through the statement catch-all, so the script
//! serializes regardless — per-name precision would be observably equivalent
//! at the dependency-graph level while complicating the analysis; (b) a parse
//! error means unparsed routine DDL cannot be ruled out, and invalid units
//! independently barrier in the Haskell layer. Out-of-script user functions
//! (pre-existing in the target database, invisible to this self-contained
//! script model which has no external routine catalog) shadowing these names
//! are assumed absent.
//!
//! One sqlparser AST-shape artifact lowers as `Expr::Function` and is NOT a
//! table member: MySQL index length prefixes such as `KEY (b(1))` parse as
//! `Expr::Function`. It is excluded structurally in index-key position only
//! (via `ignored_function_roots`, following the `PARTITION BY RANGE` scheme
//! precedent): a bare `name(N)` with a single unsigned-numeric literal is the
//! length-prefix production — functional key parts require double parens and
//! lower as `Nested` — so the shape is positional, never a name-table lookup.
//! Adding `b` to a name table would be unsound and is never done.
//!
//! Multi-dialect strategy (decided; no redesign planned):
//!
//! Upstream sqlparser-rs is syntax-only BY DESIGN and will not distinguish
//! functions by semantics (README "Syntax vs Semantics": "only a syntax
//! parser ... tries to avoid applying any SQL semantics"; semantic analysis
//! is left to downstream projects). Function semantics is therefore
//! permanently occam-owned; do not wait on or propose upstream changes.
//!
//! The current shape stays: per-dialect allowlists plus shared shape gates
//! plus the script-wide routine-DDL shadowing rule. Every entry keeps its
//! per-entry justification comment and covering tests; removals need the
//! same evidence bar as additions.
//!
//! Growth policy: census-driven selection only, with additions batched per
//! dialect documentation rather than one-off names. Generic stays
//! reserved-keyword-only: a lift there must hold on ANY engine.
//!
//! Explicitly deferred — do not build without revisiting this decision:
//! a DBMS execution matrix for function verification (heavy asset), and
//! data-file/catalog-driven tables (code tables suffice at current scale).

use crate::DialectKind;
use crate::ir;
use sqlparser::ast::{Function, ObjectNamePart, Statement};

/// Allowlisted pure builtins (lowercase canonical spelling). Every entry is a
/// scalar value function common to SQLite, PostgreSQL, and MySQL with no
/// writes, no sequence use, and no volatile/session effects. Scalar names that
/// sqlparser lowers to dedicated expressions instead of `Expr::Function`
/// (`trim` → `Expr::Trim`, `substr`/`substring` → `Expr::Substring`,
/// `ceil`/`floor` → `Expr::Ceil`/`Expr::Floor`, `case` → `Expr::Case`) never
/// fenced and are deliberately absent: there is no fence to lift.
const PURE_BUILTINS: &[&str] = &[
    "abs", // Absolute value; closed arithmetic over its argument.
    // sqlite: https://sqlite.org/lang_corefunc.html#abs | pg: https://www.postgresql.org/docs/current/functions-math.html#FUNCTIONS-MATH-FUNC-TABLE
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/mathematical-functions.html#function_abs
    "coalesce", // First-non-NULL value selection over its arguments.
    // sqlite: https://sqlite.org/lang_corefunc.html#coalesce | pg: https://www.postgresql.org/docs/current/functions-conditional.html#FUNCTIONS-COALESCE-NVL-IFNULL
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/comparison-operators.html#function_coalesce
    "concat", // Variadic string concatenation; deterministic over arguments.
    // sqlite: https://sqlite.org/lang_corefunc.html#concat | pg: https://www.postgresql.org/docs/current/functions-string.html#FUNCTIONS-STRING-OTHER
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/string-functions.html#function_concat
    "length", // String length measure (bytes vs. characters differs by dialect; every spelling is pure).
    // sqlite: https://sqlite.org/lang_corefunc.html#length | pg: https://www.postgresql.org/docs/current/functions-string.html#FUNCTIONS-STRING-OTHER
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/string-functions.html#function_length
    "lower", // Case mapping; deterministic over the argument.
    // sqlite: https://sqlite.org/lang_corefunc.html#lower | pg: https://www.postgresql.org/docs/current/functions-string.html#FUNCTIONS-STRING-SQL
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/string-functions.html#function_lower
    "nullif", // Equality-conditional NULL; a failed comparison errors rather than writes.
    // sqlite: https://sqlite.org/lang_corefunc.html#nullif | pg: https://www.postgresql.org/docs/current/functions-conditional.html#FUNCTIONS-NULLIF
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/flow-control-functions.html#function_nullif
    "round", // Numeric rounding; deterministic arithmetic.
    // sqlite: https://sqlite.org/lang_corefunc.html#round | pg: https://www.postgresql.org/docs/current/functions-math.html#FUNCTIONS-MATH-FUNC-TABLE
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/mathematical-functions.html#function_round
    "upper", // Case mapping; deterministic over the argument.
             // sqlite: https://sqlite.org/lang_corefunc.html#upper | pg: https://www.postgresql.org/docs/current/functions-string.html#FUNCTIONS-STRING-SQL
             // mysql: https://dev.mysql.com/doc/refman/8.0/en/string-functions.html#function_upper
];

/// Allowlisted pure aggregates (lowercase canonical spelling), gated by the
/// same plain-call shape as [`PURE_BUILTINS`]. Every entry is a core
/// aggregate on SQLite, PostgreSQL, and MySQL computing a closed value over
/// its input rows. Statistical siblings, ordered-set functions, and
/// string-building aggregates are deliberately absent (see the module docs).
const AGGREGATE_BUILTINS: &[&str] = &[
    "avg", // Closed arithmetic mean over the input values.
    // sqlite: https://sqlite.org/lang_aggfunc.html#avg | pg: https://www.postgresql.org/docs/current/functions-aggregate.html#FUNCTIONS-AGGREGATE-TABLE
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/aggregate-functions.html#function_avg
    "count", // Input-row counting, including `COUNT(*)`.
    // sqlite: https://sqlite.org/lang_aggfunc.html#count | pg: https://www.postgresql.org/docs/current/functions-aggregate.html#FUNCTIONS-AGGREGATE-TABLE
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/aggregate-functions.html#function_count
    "max", // Order extremum over the input values.
    // sqlite: https://sqlite.org/lang_aggfunc.html#max_agg | pg: https://www.postgresql.org/docs/current/functions-aggregate.html#FUNCTIONS-AGGREGATE-TABLE
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/aggregate-functions.html#function_max
    "min", // Order extremum over the input values.
    // sqlite: https://sqlite.org/lang_aggfunc.html#min_agg | pg: https://www.postgresql.org/docs/current/functions-aggregate.html#FUNCTIONS-AGGREGATE-TABLE
    // mysql: https://dev.mysql.com/doc/refman/8.0/en/aggregate-functions.html#function_min
    "sum", // Closed addition over the input values.
           // sqlite: https://sqlite.org/lang_aggfunc.html#sumunc | pg: https://www.postgresql.org/docs/current/functions-aggregate.html#FUNCTIONS-AGGREGATE-TABLE
           // mysql: https://dev.mysql.com/doc/refman/8.0/en/aggregate-functions.html#function_sum
];

/// Pure builtins on unknown engines (`GenericSQL`). Only SQL-standard
/// reserved-keyword special forms qualify: they cannot name user routines on
/// any conforming engine. Ordinary names — scalars and aggregates alike —
/// stay fenced on Generic (see the module docs).
const GENERIC_PURE_BUILTINS: &[&str] = &[
    "coalesce", // Keyword first-non-NULL selection; unquoted spellings only.
    // SQL:2023/2016/92 reserved; PG bars it as a function/type name (COALESCE row):
    // https://www.postgresql.org/docs/current/sql-keywords-appendix.html
    "nullif", // Keyword equality-conditional NULL; unquoted spellings only.
             // SQL:2023/2016/92 reserved; PG bars it as a function/type name (NULLIF row):
             // https://www.postgresql.org/docs/current/sql-keywords-appendix.html
];
/// Whether `statement` defines, drops, or alters a callable routine. Any such
/// statement anywhere in the script disables the pure-builtin tables for every
/// unit (see the module docs for why the coarse rule is sound and minimal).
pub(super) fn is_routine_ddl(statement: &Statement) -> bool {
    matches!(
        statement,
        Statement::CreateFunction(_)
            | Statement::CreateProcedure { .. }
            | Statement::CreateMacro { .. }
            | Statement::DropFunction(_)
            | Statement::DropProcedure { .. }
            | Statement::AlterFunction(_)
    )
}

/// Whether `ident` names a member of `table`. Matching folds through
/// [`ir::IdentifierRules`] (never a raw string compare): unquoted names fold
/// per dialect, quoted names match exactly unless the dialect folds quoted
/// identifiers too (SQLite/MySQL/Generic).
fn table_matches(
    table: &[&str],
    ident: &sqlparser::ast::Ident,
    rules: &ir::IdentifierRules,
) -> bool {
    table.iter().any(|name| {
        match rules.fold(ir::IdentifierClass::Other, ident.quote_style.is_some()) {
            ir::CaseFolding::Lower | ir::CaseFolding::Upper => {
                ident.value.eq_ignore_ascii_case(name)
            }
            ir::CaseFolding::Preserve => ident.value == *name,
        }
    })
}

/// Whether `function` is a pure-builtin call that lifts the routine fence.
/// Unknown names, volatile/sequence-adjacent names, unlisted aggregates,
/// qualified names, and window/filter/grouping call shapes all return `false`
/// (fence). SQLite, PostgreSQL, and MySQL consult the scalar and aggregate
/// tables; Generic consults only the keyword subset for unquoted spellings;
/// every other dialect returns `false`.
pub(super) fn is_pure_builtin(
    function: &Function,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> bool {
    // Window, filter, grouping, null-treatment, and parametric machinery is
    // aggregate/session-adjacent: never pure, even under a builtin name.
    // ODBC escape syntax ({fn abs(x)}) names the same function and may lift.
    // Argument shapes (DISTINCT, ORDER BY, bare subqueries) need no gating:
    // arguments are values whose reads expression analysis extracts
    // independently; ordering and duplicate treatment affect values, not
    // effects.
    if function.over.is_some()
        || function.filter.is_some()
        || !function.within_group.is_empty()
        || function.null_treatment.is_some()
        || !matches!(function.parameters, sqlparser::ast::FunctionArguments::None)
    {
        return false;
    }
    // Unqualified single-part names only: any qualification would need schema
    // resolution, so it fences.
    let [ObjectNamePart::Identifier(ident)] = function.name.0.as_slice() else {
        return false;
    };
    match dialect {
        DialectKind::SQLite | DialectKind::PostgreSql | DialectKind::MySql => {
            table_matches(PURE_BUILTINS, ident, rules)
                || table_matches(AGGREGATE_BUILTINS, ident, rules)
        }
        // The Generic claim rests on keyword status, and keywords are
        // unquoted by definition: a quoted spelling is an identifier, which
        // on an unknown engine may name a user routine. Case folds through
        // the shared matcher (Generic compares case-insensitively, as every
        // engine does for keywords).
        DialectKind::Generic => {
            ident.quote_style.is_none() && table_matches(GENERIC_PURE_BUILTINS, ident, rules)
        }
        _ => false,
    }
}

//! Native SQL parsing and semantic analysis bridge for Occam.
//!
//! Exposes C-ABI functions to tokenize, parse, and extract relational dependencies
//! (reads, writes, DDL operations, session effects) and syntactic reduction edits
//! using `sqlparser-rs` across multiple SQL dialects.

use serde::{Deserialize, Serialize};
use std::borrow::Cow;

mod actions;
pub(crate) mod binary;
mod columns;
mod edits;
mod ffi;
mod ir;
mod oracle;
mod parser;
mod source;

pub use ffi::{occam_sql_count, occam_sql_free, occam_sql_parse, occam_sql_parse_error};
pub(crate) use ir::*;

/// Supported SQL dialect variants for parser configuration and keyword folding.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
enum DialectKind {
    #[serde(rename = "postgresql")]
    PostgreSql,
    #[serde(rename = "mysql", alias = "mariadb")]
    MySql,
    #[serde(rename = "sqlite")]
    SQLite,
    #[serde(rename = "duckdb")]
    DuckDB,
    #[serde(rename = "clickhouse")]
    ClickHouse,
}

/// Root response payload returned to Occam callers.
#[derive(Serialize)]
struct NativeResponse {
    /// Initial session state derived from server configuration and dialect defaults.
    initial_session: SessionInit,
    /// Statement units parsed from the input script in sequence.
    units: Vec<NativeUnit>,
    /// Response-level column targets. Skipped when empty, so parses without
    /// `column_ir` keep the legacy payload bit-identical.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    columns: Vec<columns::ColumnTarget>,
}

/// A parsed statement unit within the SQL script, covering its source span and facts.
#[derive(Serialize)]
struct NativeUnit {
    /// Zero-based byte offset where this statement unit starts in the input.
    pub(crate) start: usize,
    /// Zero-based byte offset where this statement unit ends in the input.
    pub(crate) end: usize,
    /// Error message if sqlparser failed to parse this unit.
    pub(crate) parse_error: Option<String>,
    /// True only when the cleanly parsed AST statement is strictly on the
    /// context-independent zero-result allowlist (cannot return a query result set).
    pub(crate) cannot_produce_result: bool,
    /// Semantic facts (reads, writes, DDL ops, edit candidates) extracted from the AST.
    pub(crate) facts: Option<NativeFacts>,
}

/// Category classifying a syntactic reduction edit candidate.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Serialize)]
#[serde(rename_all = "snake_case")]
enum NativeEditCategory {
    Expression,
    Clause,
    Constraint,
    List,
    Column,
    Type,
    Literal,
}

/// A syntactic reduction edit candidate proposing a replacement over a source byte range.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
struct NativeEdit {
    /// Start byte offset in the input script.
    start: usize,
    /// End byte offset in the input script.
    end: usize,
    /// Replacement string to substitute for `[start, end)`.
    replacement: String,
    /// Classification category for reduction prioritization.
    category: NativeEditCategory,
    /// Descriptive label identifying the reduction rule (e.g. "remove-clause", "simplify-in_list").
    label: Cow<'static, str>,
    /// True only when this edit removes one or more projection items from the
    /// outermost wildcard-free SELECT of a top-level Query statement (guaranteed
    /// to strictly reduce result column arity).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    top_level_projection: bool,
    /// True when this edit rewrites a SQL single-quoted string literal.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    string_literal: bool,
    /// True when this edit rewrites a join or filter predicate inside a
    /// recursive CTE member, where weakening the predicate risks non-termination.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    recursive_predicate: bool,
    /// True when this edit is a lower-yield variant (e.g., secondary scalar
    /// literal fallback or destructive expression constant) that should trail
    /// primary structure-preserving candidates.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    low_yield: bool,
}

#[cfg(test)]
mod tests;

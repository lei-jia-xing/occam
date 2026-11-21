use serde::{Deserialize, Serialize};
use std::borrow::Cow;

mod actions;
pub(crate) mod binary;
mod columns;
mod edits;
mod ffi;
mod ir;
mod parser;
mod pure;
mod source;

pub use ffi::{occam_sql_count, occam_sql_free, occam_sql_parse, occam_sql_parse_error};
pub(crate) use ir::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
enum DialectKind {
    #[serde(rename = "generic")]
    Generic,
    #[serde(rename = "postgresql")]
    PostgreSql,
    #[serde(rename = "mysql")]
    MySql,
    #[serde(rename = "sqlite")]
    SQLite,
    #[serde(rename = "duckdb")]
    DuckDB,
    #[serde(rename = "clickhouse")]
    ClickHouse,
    #[serde(rename = "oracle")]
    Oracle,
    #[serde(rename = "ansi")]
    Ansi,
    #[serde(rename = "bigquery")]
    BigQuery,
    #[serde(rename = "databricks")]
    Databricks,
    #[serde(rename = "hive")]
    Hive,
    #[serde(rename = "mssql")]
    MsSql,
    #[serde(rename = "redshift")]
    Redshift,
    #[serde(rename = "snowflake")]
    Snowflake,
    #[serde(rename = "spark")]
    Spark,
    #[serde(rename = "teradata")]
    Teradata,
}

#[derive(Serialize)]
struct NativeResponse {
    initial_session: SessionInit,
    units: Vec<NativeUnit>,
    /// Response-level column targets. Skipped when empty, so parses without
    /// `column_ir` keep the legacy payload bit-identical.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    columns: Vec<columns::ColumnTarget>,
}

#[derive(Serialize)]
struct NativeUnit {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) parse_error: Option<String>,
    pub(crate) facts: Option<NativeFacts>,
}

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

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
struct NativeEdit {
    start: usize,
    end: usize,
    replacement: String,
    category: NativeEditCategory,
    label: Cow<'static, str>,
}

#[cfg(test)]
mod tests;

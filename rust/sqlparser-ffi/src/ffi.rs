use crate::parser::{count_units, parse_units};
use crate::{DialectKind, NativeResponse};
use std::panic::{AssertUnwindSafe, catch_unwind};
fn facts_json(sql: &str, kind: DialectKind) -> Option<Vec<u8>> {
    let units = parse_units(sql, kind).ok()?;
    serde_json::to_vec(&NativeResponse { units }).ok()
}

fn dialect_from_code(code: u32) -> Option<DialectKind> {
    match code {
        0 => Some(DialectKind::Generic),
        1 => Some(DialectKind::PostgreSql),
        2 => Some(DialectKind::MySql),
        3 => Some(DialectKind::SQLite),
        4 => Some(DialectKind::DuckDB),
        5 => Some(DialectKind::ClickHouse),
        6 => Some(DialectKind::Oracle),
        7 => Some(DialectKind::Ansi),
        8 => Some(DialectKind::BigQuery),
        9 => Some(DialectKind::Databricks),
        10 => Some(DialectKind::Hive),
        11 => Some(DialectKind::MsSql),
        12 => Some(DialectKind::Redshift),
        13 => Some(DialectKind::Snowflake),
        14 => Some(DialectKind::Spark),
        15 => Some(DialectKind::Teradata),
        _ => None,
    }
}

unsafe fn put_bytes(bytes: Vec<u8>, out_data: *mut *mut u8, out_len: *mut usize) {
    unsafe {
        let mut bytes = bytes.into_boxed_slice();
        *out_len = bytes.len();
        *out_data = bytes.as_mut_ptr();
        std::mem::forget(bytes);
    }
}

/// Parses SQL and returns its statement facts as JSON.
///
/// # Safety
/// For nonzero `len`, `data` must point to `len` readable bytes. Non-null output pointers must point to writable storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn occam_sql_parse(
    data: *const u8,
    len: usize,
    dialect_code: u32,
    out_data: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    unsafe {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if out_data.is_null() || out_len.is_null() {
                return Err(1);
            }
            if data.is_null() && len != 0 {
                return Err(1);
            }
            let kind = dialect_from_code(dialect_code).ok_or(2)?;
            let bytes = if len == 0 {
                &[][..]
            } else {
                std::slice::from_raw_parts(data, len)
            };
            let sql = std::str::from_utf8(bytes).map_err(|_| 3)?;
            let json = facts_json(sql, kind).ok_or(4)?;
            put_bytes(json, out_data, out_len);
            Ok(())
        }));
        match result {
            Ok(Err(code)) => code,
            Err(_) => 5,
            Ok(Ok(())) => 0,
        }
    }
}

/// Converts a native parser status code into an owned error message.
///
/// # Safety
/// Non-null output pointers must point to writable storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn occam_sql_parse_error(
    code: i32,
    out_data: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    unsafe {
        let message = match code {
            1 => "invalid native parser arguments",
            2 => "invalid SQL dialect code",
            3 => "SQL input is not valid UTF-8",
            4 => "native SQL parsing or processing failed",
            5 => "sqlparser native parser panicked",
            _ => "unknown sqlparser native error",
        };
        if out_data.is_null() || out_len.is_null() {
            return 1;
        }
        put_bytes(message.as_bytes().to_vec(), out_data, out_len);
        0
    }
}

/// Counts statements using the selected sqlparser dialect.
///
/// # Safety
/// For nonzero `len`, `data` must point to `len` readable bytes, and a non-null `out_count` must point to writable storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn occam_sql_count(
    data: *const u8,
    len: usize,
    dialect_code: u32,
    out_count: *mut usize,
) -> i32 {
    unsafe {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if data.is_null() && len != 0 {
                return Err(1);
            }
            if out_count.is_null() {
                return Err(1);
            }
            let kind = dialect_from_code(dialect_code).ok_or(2)?;
            let bytes = if len == 0 {
                &[][..]
            } else {
                std::slice::from_raw_parts(data, len)
            };
            let sql = std::str::from_utf8(bytes).map_err(|_| 3)?;
            let count = count_units(sql, kind).map_err(|_| 4)?;
            *out_count = count;
            Ok(())
        }));
        match result {
            Ok(Ok(())) => 0,
            Ok(Err(code)) => code,
            Err(_) => 5,
        }
    }
}

/// Frees a buffer returned by one of the native parser functions.
///
/// # Safety
/// For non-null `data`, `len` must match a buffer returned by `occam_sql_parse` or `occam_sql_parse_error`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn occam_sql_free(data: *mut u8, len: usize) {
    unsafe {
        if !data.is_null() {
            drop(Vec::from_raw_parts(data, len, len));
        }
    }
}

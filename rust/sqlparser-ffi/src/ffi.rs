use crate::ir::{
    Ident, IdentifierClass, IdentifierRules, Name, ParseConfig, SearchPathEntry, SessionInit,
};
use crate::parser::{count_units, parse_units};
use crate::{DialectKind, NativeResponse};
use std::panic::{AssertUnwindSafe, catch_unwind};

/// Folds a session identifier string according to dialect rules and quote status.
fn session_ident_with_quote(value: &str, quoted: bool, rules: &IdentifierRules) -> Ident {
    let folding = rules.fold(IdentifierClass::Relation, quoted);
    Ident(folding.apply(value))
}

/// Parse a configured identifier using the same literal syntax as a
/// `search_path` entry: an unquoted name follows the dialect's relation folding,
/// while a double-quoted name preserves case and uses `""` for an embedded
/// quote.
fn session_config_ident(value: &str, rules: &IdentifierRules) -> Ident {
    match crate::actions::literal_identifier_part(value.trim()) {
        Some((value, quoted)) => session_ident_with_quote(&value, quoted, rules),
        // Configuration is trusted input. Keep a malformed value verbatim
        // instead of silently folding quote characters into the identifier.
        None => Ident(value.to_string()),
    }
}

/// Parses a configured schema search path entry into a `SearchPathEntry` (`CurrentUser` or `Name`).
fn session_search_path_entry(value: &str, rules: &IdentifierRules) -> SearchPathEntry {
    let value = value.trim();
    if value == "$user" || value == "\"$user\"" {
        return SearchPathEntry::CurrentUser;
    }
    SearchPathEntry::Name {
        name: Name(vec![session_config_ident(value, rules)]),
    }
}

/// Initializes session state from configuration, resolving dialect-specific defaults and identifier folding.
pub(crate) fn session_init(config: &ParseConfig) -> SessionInit {
    let rules = IdentifierRules::for_dialect(config.dialect, &config.server);
    SessionInit {
        namespace_prefix: config
            .server
            .namespace_prefix
            .iter()
            .map(|part| session_config_ident(part, &rules))
            .collect(),
        search_path: config
            .server
            .search_path
            .iter()
            .map(|name| session_search_path_entry(name, &rules))
            .collect(),
        current_user: config
            .server
            .current_user
            .as_ref()
            .map(|user| session_config_ident(user, &rules)),
        autocommit: config.server.autocommit,
        foreign_key_checks: config
            .server
            .foreign_key_checks
            .unwrap_or(config.dialect != DialectKind::SQLite),
        variables: config.server.variables.clone(),
    }
}

/// Parses `sql` and encodes the full semantic response into the binary protocol format.
fn facts_binary(sql: &str, config: &ParseConfig) -> Option<Vec<u8>> {
    let units = parse_units(sql, config).ok()?;
    let columns = if config.column_bundles {
        crate::columns::assemble_columns(&units, sql)
    } else {
        Vec::new()
    };
    let response = NativeResponse {
        initial_session: session_init(config),
        units,
        columns,
    };
    Some(crate::binary::encode_native_response(&response))
}

#[allow(dead_code)]
fn facts_json(sql: &str, config: &ParseConfig) -> Option<Vec<u8>> {
    let units = parse_units(sql, config).ok()?;
    let columns = if config.column_bundles {
        crate::columns::assemble_columns(&units, sql)
    } else {
        Vec::new()
    };
    serde_json::to_vec(&NativeResponse {
        initial_session: session_init(config),
        units,
        columns,
    })
    .ok()
}

/// Maps FFI integer dialect code (0–15) to its corresponding [`DialectKind`].
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
        if bytes.is_empty() {
            *out_len = 0;
            *out_data = std::ptr::null_mut();
            return;
        }
        let mut bytes = bytes.into_boxed_slice();
        *out_len = bytes.len();
        *out_data = bytes.as_mut_ptr();
        std::mem::forget(bytes);
    }
}

/// Returns an exact native cache key (mode 0) or DuckDB replay units (mode 1).
/// An unsupported input has a null, empty result.
///
/// # Safety
/// Input must point to valid bytes of the given length, and both output
/// pointers must be writable. A nonempty result must be freed by the caller.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn occam_sql_oracle_source(
    dialect: u32,
    mode: u32,
    data: *const u8,
    len: usize,
    out_data: *mut *mut u8,
    out_len: *mut usize,
) {
    if data.is_null() || out_data.is_null() || out_len.is_null() {
        return;
    }
    unsafe {
        let payload = catch_unwind(AssertUnwindSafe(|| {
            let bytes = std::slice::from_raw_parts(data, len);
            let kind = dialect_from_code(dialect)?;
            match mode {
                0 => {
                    // Check raw leading bytes before validating full UTF-8 for native whitespace key.
                    if !matches!(kind, DialectKind::SQLite | DialectKind::DuckDB) {
                        return None;
                    }
                    let mut cursor = 0;
                    while cursor < bytes.len() && bytes[cursor].is_ascii_whitespace() {
                        cursor += 1;
                    }
                    let remaining = &bytes[cursor..];
                    if remaining.len() < 6 || !remaining[..6].eq_ignore_ascii_case(b"select") {
                        return None;
                    }
                    let sql = std::str::from_utf8(bytes).ok()?;
                    crate::oracle::cache_key(sql, kind)
                }
                1 if kind == DialectKind::DuckDB => {
                    let sql = std::str::from_utf8(bytes).ok()?;
                    crate::oracle::duckdb_replay_units_binary(sql)
                }
                _ => None,
            }
        }));
        put_bytes(
            payload.ok().flatten().unwrap_or_default(),
            out_data,
            out_len,
        );
    }
}

/// Parses SQL with a JSON-encoded `ParseConfig` and returns the binary-encoded `NativeResponse`.
///
/// # Parameters
/// - `config_data`, `config_len`: Pointer and byte length of a JSON-encoded `ParseConfig`.
/// - `data`, `len`: Pointer and byte length of the UTF-8 SQL input string.
/// - `out_data`, `out_len`: Pointers to store the allocated output buffer pointer and length.
///
/// # Return Codes
/// - `0`: Success; `*out_data` points to the binary payload and must be freed with [`occam_sql_free`].
/// - `1`: Invalid arguments (null pointers when length > 0, or null output pointers).
/// - `2`: Invalid configuration JSON.
/// - `3`: SQL input is not valid UTF-8.
/// - `4`: Native SQL parsing or fact extraction failed.
/// - `5`: Native parser panicked (caught via `catch_unwind`).
///
/// # Safety
/// Nonzero-length input buffers must point to valid readable bytes for the given length.
/// Output pointers `out_data` and `out_len` must point to valid, writable storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn occam_sql_parse(
    config_data: *const u8,
    config_len: usize,
    data: *const u8,
    len: usize,
    out_data: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    unsafe {
        let result = catch_unwind(AssertUnwindSafe(|| {
            if out_data.is_null() || out_len.is_null() {
                return Err(1);
            }
            if (data.is_null() && len != 0) || (config_data.is_null() && config_len != 0) {
                return Err(1);
            }
            let config_bytes = if config_len == 0 {
                &[][..]
            } else {
                std::slice::from_raw_parts(config_data, config_len)
            };
            let config: ParseConfig = serde_json::from_slice(config_bytes).map_err(|_| 2)?;
            let bytes = if len == 0 {
                &[][..]
            } else {
                std::slice::from_raw_parts(data, len)
            };
            let sql = std::str::from_utf8(bytes).map_err(|_| 3)?;
            let payload = facts_binary(sql, &config).ok_or(4)?;
            put_bytes(payload, out_data, out_len);
            Ok(())
        }));
        match result {
            Ok(Err(code)) => code,
            Err(_) => 5,
            Ok(Ok(())) => 0,
        }
    }
}

/// Fast path for parsing SQL with default server settings using compact integer flags,
/// avoiding JSON config serialization/deserialization on every call.
///
/// Flag bits:
/// - `1 << 0`: `chunk_edits`
/// - `1 << 1`: `column_ir`
/// - `1 << 2`: `column_bundles`
/// - `1 << 3`: `syntax_edits`
/// - `1 << 4`: `syntax_only`
///
/// # Safety
/// Same pointer validity requirements as [`occam_sql_parse`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn occam_sql_parse_flags(
    dialect_code: u32,
    flags: u32,
    data: *const u8,
    len: usize,
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
            let dialect = dialect_from_code(dialect_code).ok_or(2)?;
            let config = ParseConfig {
                dialect,
                server: Default::default(),
                chunk_edits: (flags & 1) != 0,
                column_ir: (flags & 2) != 0,
                column_bundles: (flags & 4) != 0,
                syntax_edits: (flags & 8) != 0,
                syntax_only: (flags & 16) != 0,
                span_only: (flags & 32) != 0,
            };
            let bytes = if len == 0 {
                &[][..]
            } else {
                std::slice::from_raw_parts(data, len)
            };
            let sql = std::str::from_utf8(bytes).map_err(|_| 3)?;
            let payload = facts_binary(sql, &config).ok_or(4)?;
            put_bytes(payload, out_data, out_len);
            Ok(())
        }));
        match result {
            Ok(Err(code)) => code,
            Err(_) => 5,
            Ok(Ok(())) => 0,
        }
    }
}

/// Converts a native parser status code into an owned UTF-8 error message buffer.
///
/// # Parameters
/// - `code`: The status code returned by [`occam_sql_parse`] or [`occam_sql_count`].
/// - `out_data`, `out_len`: Pointers to store the allocated message buffer pointer and length.
///
/// # Return Codes
/// - `0`: Success; `*out_data` points to the error string and must be freed with [`occam_sql_free`].
/// - `1`: Output pointers are null.
///
/// # Safety
/// Non-null output pointers must point to valid, writable storage.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn occam_sql_parse_error(
    code: i32,
    out_data: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    unsafe {
        let message = match code {
            1 => "invalid native parser arguments",
            2 => "invalid SQL parser configuration",
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

/// Counts statements in the SQL input using the selected sqlparser dialect.
///
/// # Parameters
/// - `data`, `len`: Pointer and byte length of the UTF-8 SQL input.
/// - `dialect_code`: Integer code corresponding to a [`DialectKind`].
/// - `out_count`: Pointer to receive the counted statement total.
///
/// # Return Codes
/// - `0`: Success; `*out_count` receives the statement count.
/// - `1`: Null pointers when `len > 0`, or `out_count` is null.
/// - `2`: Unknown dialect code.
/// - `3`: SQL input is not valid UTF-8.
/// - `4`: Tokenization or counting failed.
/// - `5`: Counting panicked (caught via `catch_unwind`).
///
/// # Safety
/// For nonzero `len`, `data` must point to `len` readable bytes. `out_count` must point to writable storage.
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

/// Frees a buffer allocated by [`occam_sql_parse`] or [`occam_sql_parse_error`].
///
/// # Safety
/// For non-null `data`, `len` must exactly match the length of a buffer previously allocated
/// by this library that has not yet been freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn occam_sql_free(data: *mut u8, len: usize) {
    unsafe {
        if !data.is_null() && len > 0 {
            drop(Vec::from_raw_parts(data, len, len));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn name_text(entry: SearchPathEntry) -> String {
        match entry {
            SearchPathEntry::Name { name } => name
                .0
                .iter()
                .map(|part| part.0.as_str())
                .collect::<Vec<_>>()
                .join("."),
            SearchPathEntry::CurrentUser => "$user".to_string(),
        }
    }

    #[test]
    fn config_search_path_entries_honor_quotes_and_current_user() {
        let rules = IdentifierRules::for_dialect(DialectKind::PostgreSql, &Default::default());
        assert!(matches!(
            session_search_path_entry("$user", &rules),
            SearchPathEntry::CurrentUser
        ));
        assert!(matches!(
            session_search_path_entry("\"$user\"", &rules),
            SearchPathEntry::CurrentUser
        ));
        assert_eq!(
            name_text(session_search_path_entry("\"Mixed Case\"", &rules)),
            "Mixed Case"
        );
        assert_eq!(
            name_text(session_search_path_entry("Plain", &rules)),
            "plain"
        );
    }

    #[test]
    fn dialect_codes_cover_all_dialects_exactly_once() {
        // Guards the u32 code table against duplicate and out-of-range codes.
        // Haskell's dialect round-trip exercises every u32 code through
        // countStatements/FFI into dialect_from_code: range errors fail there,
        // while a swapped-but-valid code would still pass.
        let kinds: Vec<DialectKind> = (0..16).filter_map(dialect_from_code).collect();
        assert_eq!(kinds.len(), 16);
        for (i, left) in kinds.iter().enumerate() {
            for right in &kinds[i + 1..] {
                assert_ne!(left, right);
            }
        }
        assert_eq!(dialect_from_code(16), None);
        assert_eq!(dialect_from_code(u32::MAX), None);
    }

    #[test]
    fn config_identifiers_honor_quotes_for_all_session_fields() {
        let rules = IdentifierRules::for_dialect(DialectKind::PostgreSql, &Default::default());
        assert_eq!(session_config_ident("\"MixedUser\"", &rules).0, "MixedUser");
        assert_eq!(session_config_ident("PlainUser", &rules).0, "plainuser");
        assert_eq!(
            session_config_ident("\"Mixed\"\"User\"", &rules).0,
            "Mixed\"User"
        );
        assert_eq!(session_config_ident("\"$user\"", &rules).0, "$user");

        let config = ParseConfig {
            dialect: DialectKind::PostgreSql,
            server: crate::ir::ServerConfig {
                namespace_prefix: vec!["\"Tenant\"".to_string()],
                current_user: Some("\"MixedUser\"".to_string()),
                ..Default::default()
            },
            chunk_edits: false,
            column_ir: false,
            column_bundles: false,
            syntax_edits: true,
            syntax_only: false,
            span_only: false,
        };
        let init = session_init(&config);
        assert_eq!(init.namespace_prefix[0].0, "Tenant");
        assert_eq!(init.current_user.expect("current user").0, "MixedUser");
    }

    #[test]
    fn empty_output_buffers_are_null_and_free_is_safe() {
        let mut data = std::ptr::null_mut();
        let mut len = 0;
        unsafe {
            put_bytes(Vec::new(), &mut data, &mut len);
        }
        assert!(data.is_null());
        assert_eq!(len, 0);
        unsafe {
            occam_sql_free(data, len);
        }
    }

    #[test]
    fn non_empty_output_buffers_round_trip() {
        let mut data = std::ptr::null_mut();
        let mut len = 0;
        unsafe {
            put_bytes(b"abc".to_vec(), &mut data, &mut len);
        }
        assert!(!data.is_null());
        assert_eq!(len, 3);
        let bytes = unsafe { Vec::from_raw_parts(data, len, len) };
        assert_eq!(bytes, b"abc");
    }

    fn response_json(sql: &str, column_ir: bool, column_bundles: bool) -> serde_json::Value {
        let config = ParseConfig {
            dialect: DialectKind::SQLite,
            server: crate::ir::ServerConfig::default(),
            chunk_edits: false,
            column_ir,
            column_bundles,
            syntax_edits: true,
            syntax_only: false,
            span_only: false,
        };
        let json = facts_json(sql, &config).expect("facts json");
        serde_json::from_slice(&json).expect("response json")
    }

    fn has_column_edits(response: &serde_json::Value) -> bool {
        response["units"]
            .as_array()
            .expect("units")
            .iter()
            .filter_map(|unit| unit.get("facts"))
            .filter_map(|facts| facts.get("edits"))
            .filter_map(serde_json::Value::as_array)
            .flat_map(|edits| edits.iter())
            .any(|edit| edit.get("category") == Some(&serde_json::Value::from("column")))
    }

    #[test]
    fn column_payload_gates_independently() {
        let sql = "CREATE TABLE t(a INT, b INT);\nINSERT INTO t(a, b) VALUES (1, 2);";
        // Off: legacy payload, no bundle key, no column edits.
        let off = response_json(sql, false, false);
        assert!(off.get("columns").is_none(), "no bundle key when off");
        assert!(!has_column_edits(&off), "no column edits when off");
        // Sites only: raw single-point edits, but no binding/planning payload.
        let sites = response_json(sql, true, false);
        assert!(
            sites.get("columns").is_none(),
            "no bundle key without bundles"
        );
        assert!(has_column_edits(&sites), "column singles emitted");
        // Bundles: implies collection, adds the response-level targets.
        let bundles = response_json(sql, false, true);
        let targets = bundles
            .get("columns")
            .expect("bundle key")
            .as_array()
            .expect("array");
        assert_eq!(targets.len(), 2);
        assert!(has_column_edits(&bundles), "bundles imply singles geometry");
        assert_eq!(
            targets[0].get("column"),
            Some(&serde_json::Value::from("a"))
        );
        assert!(targets[0].get("blocked").expect("blocked").is_null());
    }

    #[test]
    fn parse_flags_matches_json_config_ffi_exactly() {
        let sql = concat!(
            "CREATE TABLE t(a INT, b TEXT);\n",
            "WITH c AS (SELECT a FROM t) SELECT a FROM c;\n",
            "INSERT INTO t VALUES (1, 'hello');\n",
        );
        for &(flags, chunk_edits, column_ir, column_bundles, syntax_edits, syntax_only) in &[
            (0u32, false, false, false, false, false),
            (1u32, true, false, false, false, false),
            (8u32, false, false, false, true, false),
            (9u32, true, false, false, true, false),
            (15u32, true, true, true, true, false),
            (25u32, true, false, false, true, true),
        ] {
            let config = ParseConfig {
                dialect: DialectKind::SQLite,
                server: crate::ir::ServerConfig::default(),
                chunk_edits,
                column_ir,
                column_bundles,
                syntax_edits,
                syntax_only,
                span_only: false,
            };
            let cfg_bytes = serde_json::to_vec(&config).expect("serialize config");
            let mut json_ptr = std::ptr::null_mut();
            let mut json_len = 0;
            let rc_json = unsafe {
                occam_sql_parse(
                    cfg_bytes.as_ptr(),
                    cfg_bytes.len(),
                    sql.as_ptr(),
                    sql.len(),
                    &mut json_ptr,
                    &mut json_len,
                )
            };
            assert_eq!(rc_json, 0);
            let json_out = unsafe { std::slice::from_raw_parts(json_ptr, json_len).to_vec() };
            unsafe { occam_sql_free(json_ptr, json_len) };

            let mut flags_ptr = std::ptr::null_mut();
            let mut flags_len = 0;
            let rc_flags = unsafe {
                occam_sql_parse_flags(
                    3, // SQLite
                    flags,
                    sql.as_ptr(),
                    sql.len(),
                    &mut flags_ptr,
                    &mut flags_len,
                )
            };
            assert_eq!(rc_flags, 0);
            let flags_out = unsafe { std::slice::from_raw_parts(flags_ptr, flags_len).to_vec() };
            unsafe { occam_sql_free(flags_ptr, flags_len) };

            assert_eq!(flags_out, json_out, "flags={flags} mismatch");
        }
    }

    #[test]
    fn syntax_edits_disabled_preserves_spans_and_semantic_facts() {
        let sql = concat!(
            "CREATE TABLE t(a INT, b TEXT);\n",
            "WITH cte(x) AS (SELECT a FROM t WHERE a > 0) SELECT x FROM cte;\n",
            "INSERT INTO t VALUES (1, 'ok');\n",
            "SELECT * FROM t; SELECT 'unterminated\n",
        );
        let mk = |syntax_edits| ParseConfig {
            dialect: DialectKind::SQLite,
            server: crate::ir::ServerConfig::default(),
            chunk_edits: false,
            column_ir: false,
            column_bundles: false,
            syntax_edits,
            syntax_only: false,
            span_only: false,
        };
        let with_edits = parse_units(sql, &mk(true)).expect("with syntax_edits");
        let without_edits = parse_units(sql, &mk(false)).expect("without syntax_edits");
        assert_eq!(with_edits.len(), without_edits.len());
        for (a, b) in with_edits.iter().zip(without_edits.iter()) {
            assert_eq!(a.start, b.start);
            assert_eq!(a.end, b.end);
            assert_eq!(a.parse_error, b.parse_error);
            assert_eq!(a.cannot_produce_result, b.cannot_produce_result);
            assert_eq!(a.facts.is_some(), b.facts.is_some());
            if let (Some(fa), Some(fb)) = (&a.facts, &b.facts) {
                assert!(fb.edits.is_empty());
                assert_eq!(
                    serde_json::to_value(&fa.ops).unwrap(),
                    serde_json::to_value(&fb.ops).unwrap()
                );
                assert_eq!(
                    serde_json::to_value(&fa.barriers).unwrap(),
                    serde_json::to_value(&fb.barriers).unwrap()
                );
                assert_eq!(
                    serde_json::to_value(&fa.pre_tx).unwrap(),
                    serde_json::to_value(&fb.pre_tx).unwrap()
                );
                assert_eq!(
                    serde_json::to_value(&fa.post_tx).unwrap(),
                    serde_json::to_value(&fb.post_tx).unwrap()
                );
            }
        }
    }

    #[test]
    fn syntax_only_preserves_all_edits_and_skips_semantic_ops() {
        let sql = concat!(
            "CREATE TABLE t(a INT, b TEXT);\n",
            "INSERT INTO t VALUES (1, 'hello'), (2, 'world');\n",
            "WITH cte AS (SELECT a, b FROM t WHERE a > 0) SELECT a, b FROM cte ORDER BY a;\n",
        );
        let full = parse_units(
            sql,
            &ParseConfig {
                dialect: DialectKind::SQLite,
                server: Default::default(),
                chunk_edits: true,
                column_ir: false,
                column_bundles: false,
                syntax_edits: true,
                syntax_only: false,
                span_only: false,
            },
        )
        .expect("full parse");
        let fast = parse_units(
            sql,
            &ParseConfig {
                dialect: DialectKind::SQLite,
                server: Default::default(),
                chunk_edits: true,
                column_ir: false,
                column_bundles: false,
                syntax_edits: true,
                syntax_only: true,
                span_only: false,
            },
        )
        .expect("syntax_only parse");
        assert_eq!(full.len(), fast.len());
        for (a, b) in full.iter().zip(fast.iter()) {
            assert_eq!(a.start, b.start);
            assert_eq!(a.end, b.end);
            assert_eq!(a.cannot_produce_result, b.cannot_produce_result);
            let fa = a.facts.as_ref().expect("full facts");
            let fb = b.facts.as_ref().expect("fast facts");
            assert_eq!(fa.edits, fb.edits);
            assert!(fb.ops.is_empty());
            assert!(fb.barriers.is_empty());
        }
    }

    #[test]
    fn top_level_projection_flag_marks_only_outer_wildcard_free_select() {
        let sql = concat!(
            "WITH c AS (SELECT x, y FROM t) SELECT a, b FROM c;\n",
            "SELECT *, a FROM t;\n",
            "SELECT a, b FROM t UNION SELECT c, d FROM u;\n",
            "CREATE VIEW v AS SELECT a, b FROM t;\n",
        );
        let units = parse_units(
            sql,
            &ParseConfig {
                dialect: DialectKind::SQLite,
                server: Default::default(),
                chunk_edits: true,
                column_ir: false,
                column_bundles: false,
                syntax_edits: true,
                syntax_only: true,
                span_only: false,
            },
        )
        .expect("parse");
        // Unit 0: CTE inner SELECT (x, y) must NOT be flagged; outer SELECT (a, b) MUST be flagged.
        let u0_edits = &units[0].facts.as_ref().unwrap().edits;
        let flagged: Vec<&str> = u0_edits
            .iter()
            .filter(|e| e.top_level_projection)
            .map(|e| &sql[e.start..e.end])
            .collect();
        let unflagged_proj: Vec<&str> = u0_edits
            .iter()
            .filter(|e| !e.top_level_projection && e.label.starts_with("remove-select_expression"))
            .map(|e| &sql[e.start..e.end])
            .collect();
        assert_eq!(
            flagged.len(),
            2,
            "expected outer a and b flagged: {flagged:?}"
        );
        assert!(flagged.iter().all(|s| s.contains('a') || s.contains('b')));
        assert_eq!(
            unflagged_proj.len(),
            2,
            "expected inner CTE x and y unflagged: {unflagged_proj:?}"
        );
        // Units 1 (wildcard), 2 (UNION), 3 (CREATE VIEW): zero flagged edits.
        for idx in [1, 2, 3] {
            let edits = &units[idx].facts.as_ref().unwrap().edits;
            assert!(
                edits.iter().all(|e| !e.top_level_projection),
                "unit {idx} should have no top_level_projection edits"
            );
        }
    }
}

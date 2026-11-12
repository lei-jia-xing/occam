use crate::ir::{
    Ident, IdentifierClass, IdentifierRules, Name, ParseConfig, SearchPathEntry, SessionInit,
};
use crate::parser::{count_units, parse_units};
use crate::{DialectKind, NativeResponse};
use std::panic::{AssertUnwindSafe, catch_unwind};

fn session_ident_with_quote(value: &str, quoted: bool, rules: &IdentifierRules) -> Ident {
    let folding = rules.fold(IdentifierClass::Relation, quoted);
    Ident(match folding {
        crate::ir::CaseFolding::Lower => value.to_lowercase(),
        crate::ir::CaseFolding::Upper => value.to_uppercase(),
        crate::ir::CaseFolding::Preserve => value.to_string(),
    })
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

fn session_search_path_entry(value: &str, rules: &IdentifierRules) -> SearchPathEntry {
    let value = value.trim();
    if value == "$user" || value == "\"$user\"" {
        return SearchPathEntry::CurrentUser;
    }
    SearchPathEntry::Name {
        name: Name(vec![session_config_ident(value, rules)]),
    }
}

fn session_init(config: &ParseConfig) -> SessionInit {
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
        foreign_key_checks: config.server.foreign_key_checks,
        variables: config.server.variables.clone(),
    }
}

fn facts_json(sql: &str, config: &ParseConfig) -> Option<Vec<u8>> {
    let units = parse_units(sql, config).ok()?;
    serde_json::to_vec(&NativeResponse {
        initial_session: session_init(config),
        units,
    })
    .ok()
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

/// Parses SQL with a JSON `ParseConfig` and returns its statement facts as JSON.
///
/// # Safety
/// For nonzero lengths, the corresponding input pointers must point to readable
/// bytes. Non-null output pointers must point to writable storage.
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
            let json = facts_json(sql, &config).ok_or(4)?;
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
        };
        let init = session_init(&config);
        assert_eq!(init.namespace_prefix[0].0, "Tenant");
        assert_eq!(init.current_user.expect("current user").0, "MixedUser");
    }
}

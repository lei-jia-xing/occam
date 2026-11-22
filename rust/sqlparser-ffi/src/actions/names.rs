use super::*;
use crate::DialectKind;
use crate::ir;
use sqlparser::ast::*;

/// Splits an `ObjectName` into canonical identifier parts, applying dialect folding per component.
pub(crate) fn name_parts(
    name: &ObjectName,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> Option<Vec<ir::Ident>> {
    if name.0.is_empty() || name.0.len() > 4 {
        return None;
    }
    name.0
        .iter()
        .enumerate()
        .map(|(index, part)| match part {
            ObjectNamePart::Identifier(ident) => Some(canonical_ident(
                ident,
                rules,
                if index + 1 < name.0.len() {
                    ir::IdentifierClass::Relation
                } else {
                    class
                },
            )),
            ObjectNamePart::Function(_) => None,
        })
        .collect()
}

/// Converts an AST `ObjectName` into an IR `ir::Name`.
pub(crate) fn name_from_object(
    name: &ObjectName,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> Option<ir::Name> {
    Some(ir::Name(name_parts(name, rules, class)?))
}

/// Strips leading temporary schema qualifiers (`pg_temp`, `temp`) and returns whether the object is temporary.
pub(crate) fn strip_temp_schema(parts: &mut Vec<ir::Ident>, dialect: DialectKind) -> bool {
    if parts.len() < 2 {
        return false;
    }
    let first = parts[0].0.as_str();
    let is_temp = match dialect {
        DialectKind::PostgreSql | DialectKind::Redshift => {
            first == "pg_temp" || first.starts_with("pg_temp_")
        }
        DialectKind::SQLite | DialectKind::DuckDB => first == "temp",
        _ => false,
    };
    if is_temp {
        parts.remove(0);
    }
    is_temp
}

/// Converts identifier parts into an `ir::NameRef`, detecting temporary schema qualifiers.
pub(crate) fn relation_from_parts(mut parts: Vec<ir::Ident>, dialect: DialectKind) -> ir::NameRef {
    let temp = strip_temp_schema(&mut parts, dialect);
    ir::NameRef {
        name: ir::Name(parts),
        temp,
    }
}

/// Resolves an `ObjectName` into an `ir::NameRef` with canonical folding and temporary schema detection.
pub(crate) fn relation_ref(
    name: &ObjectName,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<ir::NameRef> {
    Some(relation_from_parts(
        name_parts(name, rules, ir::IdentifierClass::Relation)?,
        dialect,
    ))
}

/// Resolves an object definition name and returns `(canonical_name, is_temporary)`.
pub(crate) fn definition_name(
    name: &ObjectName,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
    declared_temporary: bool,
) -> Option<(ir::Name, bool)> {
    let mut parts = name_parts(name, rules, ir::IdentifierClass::Relation)?;
    let schema_temp = strip_temp_schema(&mut parts, dialect);
    Some((ir::Name(parts), declared_temporary || schema_temp))
}

/// Split a literal identifier list on `separator`, ignoring separators inside
/// double-quoted identifiers. A doubled quote inside a quoted identifier is an
/// escaped quote.
pub(crate) fn split_identifier_list(value: &str, separator: char) -> Option<Vec<&str>> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut in_quotes = false;
    let mut chars = value.char_indices().peekable();
    while let Some((index, character)) = chars.next() {
        match character {
            '"' => {
                if in_quotes && chars.peek().is_some_and(|(_, next)| *next == '"') {
                    chars.next();
                } else {
                    in_quotes = !in_quotes;
                }
            }
            c if c == separator && !in_quotes => {
                parts.push(&value[start..index]);
                start = index + c.len_utf8();
            }
            _ => {}
        }
    }
    if in_quotes {
        return None;
    }
    parts.push(&value[start..]);
    Some(parts)
}

/// Parses an identifier part from a literal string, returning `(unescaped_name, is_quoted)`.
///
/// - **Quoted identifiers**: Enclosed in `"..."`, internal whitespace is preserved, and doubled quotes
///   (`""`) unescape to a single quote (`"`). Empty quoted strings (`""`) are rejected (`None`),
///   matching PostgreSQL identifier rules.
/// - **Unquoted identifiers**: Trimmed; may not contain quotes or internal whitespace.
pub(crate) fn literal_identifier_part(part: &str) -> Option<(String, bool)> {
    let part = part.trim();
    if part.is_empty() {
        return None;
    }
    if let Some(inner) = part.strip_prefix('"') {
        let inner = inner.strip_suffix('"')?;
        let mut value = String::new();
        let mut chars = inner.chars();
        while let Some(character) = chars.next() {
            match character {
                '"' => match chars.next() {
                    Some('"') => value.push('"'),
                    _ => return None,
                },
                c => value.push(c),
            }
        }
        (!value.is_empty()).then_some((value, true))
    } else {
        let valid = !part.contains('"') && !part.chars().any(char::is_whitespace);
        valid.then(|| (part.to_string(), false))
    }
}

/// Parse the textual relation name accepted by PostgreSQL sequence functions,
/// e.g. `nextval('tenant."MixedCase"."seq"')`.
pub(crate) fn literal_relation_name(value: &str, rules: &ir::IdentifierRules) -> Option<ir::Name> {
    let mut parts = Vec::new();
    for part in split_identifier_list(value, '.')? {
        let (name, quoted) = literal_identifier_part(part)?;
        let ident = if quoted {
            Ident::with_quote('"', name.as_str())
        } else {
            Ident::new(name.as_str())
        };
        parts.push(canonical_ident(
            &ident,
            rules,
            ir::IdentifierClass::Relation,
        ));
    }
    Some(ir::Name(parts))
}

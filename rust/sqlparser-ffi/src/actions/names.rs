use super::*;
use crate::DialectKind;
use crate::ir;
use sqlparser::ast::*;

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

pub(crate) fn name_from_object(
    name: &ObjectName,
    rules: &ir::IdentifierRules,
    class: ir::IdentifierClass,
) -> Option<ir::Name> {
    Some(ir::Name(name_parts(name, rules, class)?))
}

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
        true
    } else {
        false
    }
}

pub(crate) fn relation_from_parts(mut parts: Vec<ir::Ident>, dialect: DialectKind) -> ir::NameRef {
    let temp = strip_temp_schema(&mut parts, dialect);
    ir::NameRef {
        name: ir::Name(parts),
        temp,
    }
}

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

/// Parse one identifier from a string literal. Unquoted identifiers may not
/// contain whitespace or quotes; quoted identifiers preserve whitespace and
/// use doubled quotes for an embedded quote. Zero-length quoted identifiers
/// are rejected, matching PostgreSQL.
pub(crate) fn literal_identifier_part(part: &str) -> Option<(String, bool)> {
    let part = part.trim();
    if part.is_empty() {
        return None;
    }
    if part.starts_with('"') {
        if part.len() < 2 || !part.ends_with('"') {
            return None;
        }
        let mut value = String::new();
        let mut chars = part[1..part.len() - 1].chars();
        while let Some(character) = chars.next() {
            if character == '"' {
                if chars.next() == Some('"') {
                    value.push('"');
                } else {
                    return None;
                }
            } else {
                value.push(character);
            }
        }
        if value.is_empty() {
            return None;
        }
        Some((value, true))
    } else {
        if part.contains('"') || part.chars().any(char::is_whitespace) {
            return None;
        }
        Some((part.to_string(), false))
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

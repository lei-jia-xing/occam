use super::*;
use crate::DialectKind;
use crate::ir;
use sqlparser::ast::*;

pub(crate) fn sequence_ref_from_expr(
    expr: &Expr,
    rules: &ir::IdentifierRules,
) -> Option<ir::NameRef> {
    match expr {
        Expr::Value(value) => match &value.value {
            Value::SingleQuotedString(name)
            | Value::TripleSingleQuotedString(name)
            | Value::EscapedStringLiteral(name) => {
                literal_relation_name(name, rules).map(|name| ir::NameRef { name, temp: false })
            }
            _ => None,
        },
        Expr::Cast {
            kind: CastKind::Cast | CastKind::DoubleColon,
            expr,
            data_type: DataType::Regclass | DataType::Text,
            format: None,
        }
        | Expr::Nested(expr) => sequence_ref_from_expr(expr, rules),
        _ => None,
    }
}

pub(crate) enum SequenceEffect {
    Write(ir::NameRef),
    Read(ir::NameRef),
    Unknown,
}

pub(crate) fn sequence_function_effect(
    function: &Function,
    rules: &ir::IdentifierRules,
) -> Option<SequenceEffect> {
    let ident = match function.name.0.last()? {
        ObjectNamePart::Identifier(ident) => ident,
        ObjectNamePart::Function(_) => return None,
    };
    let name = ["nextval", "setval", "currval", "lastval"]
        .into_iter()
        .find(|name| ident.value.eq_ignore_ascii_case(name))?;
    // Only explicit pg_catalog qualification and the builtin spelling prove
    // meaning; unqualified names may resolve to user-defined overloads.
    let builtin_ident = |ident: &Ident, expected: &str| {
        if ident.quote_style.is_some() {
            ident.value == expected
        } else {
            ident.value.eq_ignore_ascii_case(expected)
        }
    };
    let builtin = builtin_ident(ident, name)
        && match function.name.0.as_slice() {
            [ObjectNamePart::Identifier(schema), _] => builtin_ident(schema, "pg_catalog"),
            _ => false,
        };
    if !builtin {
        return Some(SequenceEffect::Unknown);
    }
    let arguments = match &function.args {
        FunctionArguments::List(list)
            if list.duplicate_treatment.is_none() && list.clauses.is_empty() =>
        {
            &list.args
        }
        _ => return Some(SequenceEffect::Unknown),
    };
    let valid_arity = match name {
        "nextval" | "currval" => arguments.len() == 1,
        "setval" => matches!(arguments.len(), 2 | 3),
        // lastval depends on whichever sequence last advanced in this session.
        _ => false,
    };
    if !valid_arity {
        return Some(SequenceEffect::Unknown);
    }
    let reference = match &arguments[0] {
        FunctionArg::Unnamed(FunctionArgExpr::Expr(expr)) => sequence_ref_from_expr(expr, rules),
        _ => None,
    };
    Some(match reference {
        Some(reference) if name == "currval" => SequenceEffect::Read(reference),
        Some(reference) => SequenceEffect::Write(reference),
        None => SequenceEffect::Unknown,
    })
}

pub(crate) fn sequence_expr_effect(
    expr: &Expr,
    dialect: DialectKind,
    rules: &ir::IdentifierRules,
) -> Option<SequenceEffect> {
    match expr {
        Expr::Function(function) if dialect == DialectKind::PostgreSql => {
            sequence_function_effect(function, rules)
        }
        _ => None,
    }
}

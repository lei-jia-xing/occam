use crate::actions::analyze_statement;
use crate::edits::AnalysisContext;
use crate::ir::{IdentifierRules, ParseConfig};
use crate::source::{ByteSpan, SourceMap, TokenKind, dialect, token_positions};
use crate::{DialectKind, NativeUnit};
use sqlparser::ast::{Statement, Value};
use sqlparser::keywords::Keyword;
use sqlparser::parser::{Parser, ParserError};
use sqlparser::tokenizer::{Token, Tokenizer};

/// Accept SQLite's bare boolean arguments for foreign_keys with their original spans.
fn parse_sqlite_foreign_keys(parser: &mut Parser<'_>) -> Option<Result<Statement, ParserError>> {
    let token = |n| &parser.peek_nth_token_ref(n).token;
    if !matches!(token(0), Token::Word(word) if word.keyword == Keyword::PRAGMA) {
        return None;
    }
    let name_index = if matches!(token(1), Token::Word(word) if word.value.eq_ignore_ascii_case("main") || word.value.eq_ignore_ascii_case("temp"))
        && *token(2) == Token::Period
    {
        3
    } else {
        1
    };
    if !matches!(token(name_index), Token::Word(word) if word.value.eq_ignore_ascii_case("foreign_keys"))
    {
        return None;
    }
    let is_eq = match token(name_index + 1) {
        Token::Eq => true,
        Token::LParen if *token(name_index + 3) == Token::RParen => false,
        _ => return None,
    };
    let argument = parser.peek_nth_token_ref(name_index + 2);
    let Token::Word(word) = &argument.token else {
        return None;
    };
    let enabled = match word.value.to_ascii_lowercase().as_str() {
        "on" | "yes" | "true" if word.quote_style.is_none() => true,
        "off" | "no" | "false" if word.quote_style.is_none() => false,
        _ => return None,
    };
    let value = Value::Boolean(enabled).with_span(argument.span);
    parser.advance_token(); // PRAGMA
    Some(parser.parse_object_name(false).map(|name| {
        parser.advance_token(); // = or (
        parser.advance_token(); // boolean argument
        if !is_eq {
            parser.advance_token(); // )
        }
        Statement::Pragma {
            name,
            value: Some(value),
            is_eq,
        }
    }))
}

/// Parses all statement units from `source` according to `config`, extracting semantic facts and source spans.
pub(super) fn parse_units(source: &str, config: &ParseConfig) -> Result<Vec<NativeUnit>, String> {
    let mut units = Vec::new();
    walk_units(source, config, true, |unit| units.push(unit))?;
    Ok(units)
}

/// Counts the number of statement units in `source` for the specified dialect without full semantic analysis.
pub(super) fn count_units(source: &str, kind: DialectKind) -> Result<usize, String> {
    walk_units(
        source,
        &ParseConfig {
            dialect: kind,
            server: Default::default(),
            chunk_edits: false,
            column_ir: false,
            column_bundles: false,
            syntax_edits: false,
            syntax_only: false,
        },
        false,
        |_| {},
    )
}

/// One parsed statement awaiting analysis. Statements are analyzed only after
/// the whole script is parsed so routine DDL anywhere in the script can
/// disable the pure-builtin table script-wide (coarse shadowing rule).
struct PendingUnit {
    start: usize,
    end: usize,
    statement: Option<Statement>,
    parse_error: Option<String>,
    has_parse_error: bool,
}

/// Tokenizes and walks statement units in `source`.
///
/// In `detailed` mode, records exact byte offsets and executes AST analysis on each unit;
/// in non-detailed mode, only counts statement boundaries.
fn walk_units(
    source: &str,
    config: &ParseConfig,
    detailed: bool,
    mut emit: impl FnMut(NativeUnit),
) -> Result<usize, String> {
    let kind = config.dialect;
    let rules = IdentifierRules::for_dialect(kind, &config.server);
    let dialect = dialect(kind);
    let mut tokenizer = Tokenizer::new(dialect, source);
    let mut tokens = Vec::with_capacity((source.len() / 4).max(16));
    let tokenizer_error = tokenizer.tokenize_with_location_into_buf(&mut tokens).err();
    let source_map = detailed.then(|| SourceMap::new(source));
    let need_tokens =
        detailed && (config.syntax_edits || config.column_ir || config.column_bundles);
    let mut positions = match (&source_map, need_tokens) {
        (Some(source_map), true) => token_positions(&tokens, source_map)?,
        _ => Vec::new(),
    };
    let lexical_suffix = match (&tokenizer_error, detailed, need_tokens) {
        (Some(error), true, true) => {
            let prefix_len = positions
                .iter()
                .rposition(|token| matches!(token.kind, TokenKind::Semicolon))
                .map_or(0, |index| index + 1);
            let start = positions
                .iter()
                .skip(prefix_len)
                .find(|token| !matches!(token.kind, TokenKind::Whitespace))
                .map(|token| token.start)
                .unwrap_or_else(|| positions.last().map_or(0, |token| token.end));
            if start >= source.len() {
                return Err("sqlparser returned an empty tokenizer error unit".to_string());
            }
            tokens.truncate(prefix_len);
            positions.truncate(prefix_len);
            Some((start, Some(error.to_string())))
        }
        (Some(error), true, false) => {
            let source_map = source_map
                .as_ref()
                .ok_or_else(|| "source_map missing in detailed parse mode".to_string())?;
            let prefix_len = tokens
                .iter()
                .rposition(|token| token.token == Token::SemiColon)
                .map_or(0, |index| index + 1);
            let start = match tokens
                .iter()
                .skip(prefix_len)
                .find(|token| !matches!(token.token, Token::Whitespace(_)))
            {
                Some(token) => {
                    source_map
                        .span(token.span)
                        .ok_or_else(|| "sqlparser returned an unmappable token span".to_string())?
                        .start
                }
                None => match tokens.last() {
                    Some(token) => {
                        source_map
                            .span(token.span)
                            .ok_or_else(|| {
                                "sqlparser returned an unmappable token span".to_string()
                            })?
                            .end
                    }
                    None => 0,
                },
            };
            if start >= source.len() {
                return Err("sqlparser returned an empty tokenizer error unit".to_string());
            }
            tokens.truncate(prefix_len);
            Some((start, Some(error.to_string())))
        }
        (Some(_), false, _) => {
            let prefix_len = tokens
                .iter()
                .rposition(|token| token.token == Token::SemiColon)
                .map_or(0, |index| index + 1);
            tokens.truncate(prefix_len);
            Some((0, None))
        }
        (None, _, _) => None,
    };

    // The parser retains the tokens used for delimiter recovery. Detailed
    // parsing alone needs byte positions and structural token metadata.
    let token_count = tokens.len();
    let delimiter_at = |parser: &Parser<'_>, index| {
        let token = parser.token_at(index);
        (token.token == Token::SemiColon).then_some(token.span.start)
    };
    let mut parser = Parser::new(dialect).with_tokens_with_locations(tokens);
    let mut count = 0;
    let mut pending: Vec<PendingUnit> = if detailed {
        Vec::with_capacity(16)
    } else {
        Vec::new()
    };
    loop {
        while parser.peek_token_ref().token == Token::SemiColon {
            parser.advance_token();
        }
        let first = parser.peek_token_ref();
        if first.token == Token::EOF {
            break;
        }
        let first_location = first.span.start;
        let start = match &source_map {
            Some(source_map) => source_map
                .offset(first_location)
                .ok_or_else(|| "sqlparser returned an unmappable statement start".to_string())?,
            None => 0,
        };
        let parsed = if kind == DialectKind::SQLite {
            parse_sqlite_foreign_keys(&mut parser).unwrap_or_else(|| parser.parse_statement())
        } else {
            parser.parse_statement()
        };
        let mut parse_error = None;
        let mut has_parse_error = false;
        let statement = match parsed {
            Ok(statement) => {
                let current_index = parser.get_current_index();
                let delimiter_consumed = delimiter_at(&parser, current_index).is_some();
                let next = parser.peek_token_ref();
                let missing_delimiter = !delimiter_consumed
                    && next.token != Token::EOF
                    && next.token != Token::SemiColon;
                if missing_delimiter {
                    while parser.peek_token_ref().token != Token::EOF
                        && parser.peek_token_ref().token != Token::SemiColon
                    {
                        parser.advance_token();
                    }
                    has_parse_error = true;
                    if detailed {
                        parse_error = Some("expected a semicolon between statements".to_string());
                    }
                }
                Some(statement)
            }
            Err(error) => {
                let current_index = parser.get_current_index();
                let delimiter_consumed = delimiter_at(&parser, current_index)
                    .is_some_and(|location| location >= first_location);
                if !delimiter_consumed {
                    while parser.peek_token_ref().token != Token::EOF
                        && parser.peek_token_ref().token != Token::SemiColon
                    {
                        parser.advance_token();
                    }
                }
                has_parse_error = true;
                if detailed {
                    parse_error = Some(error.to_string());
                }
                None
            }
        };

        let current_index = parser.get_current_index();
        let delimiter_consumed =
            delimiter_at(&parser, current_index).is_some_and(|location| location >= first_location);
        let delimiter_pending = parser.peek_token_ref().token == Token::SemiColon;
        let at_eof = parser.peek_token_ref().token == Token::EOF;
        // In detailed mode, determine the unit's end byte offset:
        // - At EOF on unparsed trailing content, span to the end of the input source.
        // - Otherwise, find the last substantive token (excluding trailing whitespace/semicolons/EOF).
        let end = match (detailed, need_tokens) {
            (true, _) if at_eof && statement.is_none() && lexical_suffix.is_none() => source.len(),
            (true, true) => positions
                .iter()
                .take(current_index.saturating_add(1))
                .rev()
                .find(|token| {
                    token.start >= start
                        && !matches!(
                            token.kind,
                            TokenKind::Whitespace | TokenKind::Semicolon | TokenKind::Eof
                        )
                })
                .map(|token| token.end)
                .ok_or_else(|| "sqlparser returned an unmappable statement end".to_string())?,
            (true, false) => {
                let source_map = source_map
                    .as_ref()
                    .ok_or_else(|| "source_map missing in detailed parse mode".to_string())?;
                let mut found = None;
                if token_count > 0 {
                    let max_idx = current_index.min(token_count - 1);
                    for idx in (0..=max_idx).rev() {
                        let tok = parser.token_at(idx);
                        if matches!(
                            tok.token,
                            Token::Whitespace(_) | Token::SemiColon | Token::EOF
                        ) {
                            continue;
                        }
                        let span = source_map.span(tok.span).ok_or_else(|| {
                            "sqlparser returned an unmappable statement end".to_string()
                        })?;
                        if span.start < start {
                            break;
                        }
                        found = Some(span.end);
                        break;
                    }
                }
                found.ok_or_else(|| "sqlparser returned an unmappable statement end".to_string())?
            }
            (false, _) => 0,
        };
        if detailed && end < start {
            return Err("sqlparser returned an invalid statement boundary".to_string());
        }
        if detailed {
            pending.push(PendingUnit {
                start,
                end,
                statement,
                parse_error,
                has_parse_error,
            });
        }
        count += 1;
        if delimiter_pending {
            parser.advance_token();
        } else if !delimiter_consumed && !at_eof {
            return Err("sqlparser failed to recover at a statement boundary".to_string());
        }
    }

    // Coarse shadowing rule: any routine DDL or parse error anywhere in the
    // script disables the pure-builtin table for every unit, restoring the
    // blanket routine fence. Analysis runs only now so the flag is known.
    let pure_table_enabled = detailed
        && lexical_suffix.is_none()
        && !pending.iter().any(|unit| unit.has_parse_error)
        && !pending.iter().any(|unit| {
            unit.statement
                .as_ref()
                .is_some_and(crate::pure::is_routine_ddl)
        });
    if detailed {
        let source_map = source_map
            .as_ref()
            .ok_or_else(|| "source_map missing in detailed parse mode".to_string())?;
        let mut pos_cursor = 0usize;
        for unit in pending {
            let PendingUnit {
                start,
                end,
                statement,
                parse_error,
                has_parse_error,
            } = unit;
            let facts = if has_parse_error {
                None
            } else {
                let statement = statement.as_ref().ok_or_else(|| {
                    "sqlparser returned no statement for error-free unit".to_string()
                })?;
                let unit_tokens = if need_tokens {
                    let rel_start =
                        positions[pos_cursor..].partition_point(|token| token.start < start);
                    let token_start = pos_cursor + rel_start;
                    let rel_end =
                        positions[token_start..].partition_point(|token| token.end <= end);
                    let token_end = token_start + rel_end;
                    pos_cursor = token_end;
                    &positions[token_start..token_end]
                } else {
                    &[]
                };
                let facts = analyze_statement(
                    statement,
                    AnalysisContext {
                        dialect: kind,
                        rules: &rules,
                        source,
                        source_map,
                        tokens: unit_tokens,
                        unit: ByteSpan { start, end },
                        chunk_edits: config.chunk_edits,
                        column_ir: config.column_ir || config.column_bundles,
                        syntax_edits: config.syntax_edits,
                        syntax_only: config.syntax_only,
                        pure_table_enabled,
                    },
                );
                for edit in &facts.edits {
                    if edit.start < start
                        || edit.start >= edit.end
                        || edit.end > end
                        || !source.is_char_boundary(edit.start)
                        || !source.is_char_boundary(edit.end)
                    {
                        return Err(format!(
                            "sqlparser returned invalid edit {} [{}..{}) for unit [{start}..{end})",
                            edit.label, edit.start, edit.end
                        ));
                    }
                }
                Some(facts)
            };
            let cannot_produce_result = tokenizer_error.is_none()
                && !has_parse_error
                && statement
                    .as_ref()
                    .is_some_and(statement_cannot_produce_result);
            emit(NativeUnit {
                start,
                end,
                parse_error,
                cannot_produce_result,
                facts,
            });
        }
    }

    if let Some((start, parse_error)) = lexical_suffix {
        if detailed {
            emit(NativeUnit {
                start,
                end: source.len(),
                parse_error,
                cannot_produce_result: false,
                facts: None,
            });
        }
        count += 1;
    }
    Ok(count)
}

/// Strict allowlist of top-level SQL AST statements whose grammar production
/// cannot return a query result set in SQLite or DuckDB regardless of prior
/// schema, triggers, views, or session context.
fn statement_cannot_produce_result(statement: &Statement) -> bool {
    match statement {
        Statement::CreateTable(create) => create.query.is_none(),
        Statement::CreateIndex(_)
        | Statement::CreateView(_)
        | Statement::CreateTrigger(_)
        | Statement::CreateSchema { .. }
        | Statement::Drop { .. }
        | Statement::DropTrigger(_)
        | Statement::AlterTable(_)
        | Statement::Commit { .. }
        | Statement::Rollback { .. }
        | Statement::Savepoint { .. }
        | Statement::ReleaseSavepoint { .. } => true,
        Statement::StartTransaction { statements, .. } => statements.is_empty(),
        Statement::Insert(insert) => insert.returning.is_none() && insert.output.is_none(),
        Statement::Update(update) => update.returning.is_none() && update.output.is_none(),
        Statement::Delete(delete) => delete.returning.is_none() && delete.output.is_none(),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn classify(dialect: DialectKind, sql: &str) -> Vec<bool> {
        let units = parse_units(
            sql,
            &ParseConfig {
                dialect,
                server: Default::default(),
                chunk_edits: false,
                column_ir: false,
                column_bundles: false,
                syntax_edits: false,
                syntax_only: false,
            },
        )
        .expect("parse should succeed");
        units.into_iter().map(|u| u.cannot_produce_result).collect()
    }

    #[test]
    fn non_result_allowlist_classifies_ddl_dml_and_excludes_queries_and_returning() {
        let sql = "\
CREATE TABLE t(x INT);
CREATE TABLE ctas AS SELECT 1;
INSERT INTO t VALUES (1);
INSERT INTO t VALUES (2) RETURNING x;
UPDATE t SET x = 3;
UPDATE t SET x = 4 RETURNING x;
DELETE FROM t WHERE x = 1;
DELETE FROM t WHERE x = 2 RETURNING x;
CREATE INDEX idx_t ON t(x);
BEGIN;
SAVEPOINT s;
ROLLBACK TO s;
RELEASE s;
COMMIT;
PRAGMA foreign_keys = ON;
SELECT x FROM t;
EXPLAIN SELECT 1;";
        let flags = classify(DialectKind::SQLite, sql);
        assert_eq!(
            flags,
            vec![
                true,  // CREATE TABLE
                false, // CREATE TABLE AS SELECT (excluded)
                true,  // INSERT
                false, // INSERT RETURNING (excluded)
                true,  // UPDATE
                false, // UPDATE RETURNING (excluded)
                true,  // DELETE
                false, // DELETE RETURNING (excluded)
                true,  // CREATE INDEX
                true,  // BEGIN
                true,  // SAVEPOINT
                true,  // ROLLBACK TO
                true,  // RELEASE
                true,  // COMMIT
                false, // PRAGMA (excluded)
                false, // SELECT (excluded)
                false, // EXPLAIN (excluded)
            ]
        );
    }
}

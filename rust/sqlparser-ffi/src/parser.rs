use crate::actions::analyze_statement;
use crate::edits::AnalysisContext;
use crate::ir::{IdentifierRules, ParseConfig};
use crate::source::{ByteSpan, SourceMap, TokenKind, dialect, token_positions};
use crate::{DialectKind, NativeUnit};
use sqlparser::parser::Parser;
use sqlparser::tokenizer::{Token, Tokenizer};
pub(super) fn parse_units(source: &str, config: &ParseConfig) -> Result<Vec<NativeUnit>, String> {
    let mut units = Vec::new();
    walk_units(source, config, true, |unit| units.push(unit))?;
    Ok(units)
}

pub(super) fn count_units(source: &str, kind: DialectKind) -> Result<usize, String> {
    walk_units(
        source,
        &ParseConfig {
            dialect: kind,
            server: Default::default(),
        },
        false,
        |_| {},
    )
}

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
    let mut tokens = Vec::new();
    let tokenizer_error = tokenizer.tokenize_with_location_into_buf(&mut tokens).err();
    let source_map = detailed.then(|| SourceMap::new(source));
    let mut positions = token_positions(&tokens, source_map.as_ref())?;
    let mut lexical_suffix: Option<(usize, Option<String>)> = None;
    if let Some(error) = &tokenizer_error {
        let prefix_len = positions
            .iter()
            .rposition(|token| matches!(token.kind, TokenKind::Semicolon))
            .map_or(0, |index| index + 1);
        if detailed {
            let map = source_map.as_ref().unwrap();
            let error_start = map.offset(error.location).ok_or_else(|| {
                "sqlparser returned an unmappable tokenizer error location".to_string()
            })?;
            let start = positions
                .iter()
                .skip(prefix_len)
                .find(|token| !matches!(token.kind, TokenKind::Whitespace))
                .map(|token| token.start)
                .unwrap_or(error_start);
            if start >= source.len() {
                return Err("sqlparser returned an empty tokenizer error unit".to_string());
            }
            lexical_suffix = Some((start, Some(error.to_string())));
        } else {
            lexical_suffix = Some((0, None));
        }
        tokens.truncate(prefix_len);
        positions.truncate(prefix_len);
    }

    let mut parser = Parser::new(dialect).with_tokens_with_locations(tokens);
    let mut count = 0;
    loop {
        while parser.peek_token_ref().token == Token::SemiColon {
            parser.advance_token();
        }
        let first = parser.peek_token_ref();
        if first.token == Token::EOF {
            break;
        }
        let first_location = first.span.start;
        let first_index = positions.partition_point(|token| token.span.start < first_location);
        let Some(first_position) = positions.get(first_index).filter(|token| {
            token.span.start == first_location && !matches!(token.kind, TokenKind::Whitespace)
        }) else {
            return Err("sqlparser returned an unmappable statement start".to_string());
        };
        let start = first_position.start;
        let parsed = parser.parse_statement();
        let mut parse_error = None;
        let mut has_parse_error = false;
        let statement = match parsed {
            Ok(statement) => {
                let current_index = parser.get_current_index();
                let delimiter_consumed = positions
                    .get(current_index)
                    .is_some_and(|token| matches!(token.kind, TokenKind::Semicolon));
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
                let delimiter_consumed = positions.get(current_index).is_some_and(|token| {
                    matches!(token.kind, TokenKind::Semicolon) && token.span.start >= first_location
                });
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
        let delimiter_consumed = positions.get(current_index).is_some_and(|token| {
            matches!(token.kind, TokenKind::Semicolon) && token.span.start >= first_location
        });
        let delimiter_pending = parser.peek_token_ref().token == Token::SemiColon;
        let at_eof = parser.peek_token_ref().token == Token::EOF;
        let end = if detailed {
            if at_eof && statement.is_none() && lexical_suffix.is_none() {
                source.len()
            } else {
                positions
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
                    .ok_or_else(|| "sqlparser returned an unmappable statement end".to_string())?
            }
        } else {
            0
        };
        if detailed && end < start {
            return Err("sqlparser returned an invalid statement boundary".to_string());
        }
        if detailed {
            let facts = if has_parse_error {
                None
            } else {
                let statement = statement.as_ref().ok_or_else(|| {
                    "sqlparser returned no statement for error-free unit".to_string()
                })?;
                let source_map = source_map
                    .as_ref()
                    .ok_or_else(|| "source_map missing in detailed parse mode".to_string())?;
                let token_start = positions.partition_point(|token| token.start < start);
                let token_end = positions.partition_point(|token| token.end <= end);
                let facts = analyze_statement(
                    statement,
                    AnalysisContext {
                        dialect: kind,
                        rules: &rules,
                        source,
                        source_map,
                        tokens: &positions[token_start..token_end],
                        unit: ByteSpan { start, end },
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
            emit(NativeUnit {
                start,
                end,
                parse_error,
                facts,
            });
        }
        count += 1;
        if delimiter_pending {
            parser.advance_token();
        } else if !delimiter_consumed && !at_eof {
            return Err("sqlparser failed to recover at a statement boundary".to_string());
        }
    }

    if let Some((start, parse_error)) = lexical_suffix {
        if detailed {
            emit(NativeUnit {
                start,
                end: source.len(),
                parse_error,
                facts: None,
            });
        }
        count += 1;
    }
    Ok(count)
}

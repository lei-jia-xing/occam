//! Source proofs for the native result oracles. These small grammar subsets
//! have no routine lookup, session effects, or references to SQL source text.
use crate::DialectKind;
use crate::source::{SourceMap, dialect};
use serde::Serialize;
use sqlparser::keywords::Keyword;
use sqlparser::tokenizer::{Token, TokenWithSpan, Tokenizer, Whitespace};

fn tokens(sql: &str, kind: DialectKind) -> Option<Vec<TokenWithSpan>> {
    if sql.as_bytes().contains(&0) {
        return None;
    }
    let mut tokens = Tokenizer::new(dialect(kind), sql)
        .tokenize_with_location()
        .ok()?;
    // Keep comments out of the recognized grammar. They remain byte-exact in
    // the ordinary candidate key, including dialect-specific comment syntax.
    tokens.retain(|t| {
        !matches!(
            t.token,
            Token::Whitespace(Whitespace::Space | Whitespace::Tab | Whitespace::Newline)
        )
    });
    Some(tokens)
}

fn keyword(token: &TokenWithSpan, expected: Keyword) -> bool {
    matches!(&token.token, Token::Word(w) if w.quote_style.is_none() && w.keyword == expected)
}

fn take_token(input: &mut &[TokenWithSpan], expected: Token) -> Option<()> {
    let (first, rest) = input.split_first()?;
    if first.token != expected {
        return None;
    }
    *input = rest;
    Some(())
}

fn take_keyword(input: &mut &[TokenWithSpan], expected: Keyword) -> Option<()> {
    let (first, rest) = input.split_first()?;
    if !keyword(first, expected) {
        return None;
    }
    *input = rest;
    Some(())
}

fn take_name<'a>(input: &mut &'a [TokenWithSpan]) -> Option<&'a str> {
    let (first, rest) = input.split_first()?;
    let Token::Word(w) = &first.token else {
        return None;
    };
    if w.keyword != Keyword::NoKeyword || w.quote_style.is_some() || !w.value.is_ascii() {
        return None;
    }
    *input = rest;
    Some(&w.value)
}

fn take_literal(input: &mut &[TokenWithSpan]) -> Option<()> {
    let (mut first, mut rest) = input.split_first()?;
    if matches!(first.token, Token::Plus | Token::Minus) {
        (first, rest) = rest.split_first()?;
        if !matches!(first.token, Token::Number(..)) {
            return None;
        }
    }
    match &first.token {
        Token::Number(n, false) if !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit()) => (),
        Token::SingleQuotedString(_) => (),
        Token::Word(w)
            if w.quote_style.is_none()
                && matches!(w.keyword, Keyword::NULL | Keyword::TRUE | Keyword::FALSE) => {}
        _ => return None,
    }
    *input = rest;
    Some(())
}

fn separated(
    input: &mut &[TokenWithSpan],
    item: impl Fn(&mut &[TokenWithSpan]) -> Option<()>,
) -> Option<()> {
    item(input)?;
    while take_token(input, Token::Comma).is_some() {
        item(input)?;
    }
    Some(())
}

fn scalar_select(mut input: &[TokenWithSpan]) -> Option<()> {
    take_keyword(&mut input, Keyword::SELECT)?;
    separated(&mut input, |input| {
        take_literal(input)?;
        take_keyword(input, Keyword::AS)?;
        take_name(input)?;
        Some(())
    })?;
    input.is_empty().then_some(())
}

pub(super) fn cache_key(sql: &str, kind: DialectKind) -> Option<Vec<u8>> {
    if !matches!(kind, DialectKind::SQLite | DialectKind::DuckDB) {
        return None;
    }
    if !sql
        .trim_ascii_start()
        .as_bytes()
        .get(..6)?
        .eq_ignore_ascii_case(b"select")
    {
        return None;
    }
    let tokens = tokens(sql, kind)?;
    let bare = tokens
        .split_last()
        .filter(|(last, _)| last.token == Token::SemiColon)
        .map_or(tokens.as_slice(), |(_, prefix)| prefix);
    scalar_select(bare)?;
    let source = SourceMap::new(sql);
    let mut key = Vec::new();
    let mut previous_end = 0;
    for token in tokens {
        let span = source.span(token.span)?;
        if span.start == span.end {
            return None;
        }
        // Spans address the original SQL, including literal spelling.
        if !sql
            .get(previous_end..span.start)?
            .bytes()
            .all(|b| b.is_ascii_whitespace())
        {
            return None;
        }
        key.extend_from_slice(sql.get(span.start..span.end)?.as_bytes());
        key.push(0);
        previous_end = span.end;
    }
    sql.get(previous_end..)?
        .bytes()
        .all(|b| b.is_ascii_whitespace())
        .then_some(key)
}

fn create_table(mut input: &[TokenWithSpan]) -> Option<&str> {
    take_keyword(&mut input, Keyword::CREATE)?;
    take_keyword(&mut input, Keyword::TABLE)?;
    let name = take_name(&mut input)?;
    take_token(&mut input, Token::LParen)?;
    separated(&mut input, |input| {
        take_name(input)?;
        let (data_type, rest) = input.split_first()?;
        let Token::Word(w) = &data_type.token else {
            return None;
        };
        if w.quote_style.is_some()
            || !matches!(
                w.keyword,
                Keyword::TINYINT
                    | Keyword::SMALLINT
                    | Keyword::INT
                    | Keyword::INTEGER
                    | Keyword::BIGINT
                    | Keyword::HUGEINT
                    | Keyword::VARCHAR
                    | Keyword::BOOLEAN
            )
        {
            return None;
        }
        *input = rest;
        Some(())
    })?;
    take_token(&mut input, Token::RParen)?;
    input.is_empty().then_some(name)
}

fn insert_values(mut input: &[TokenWithSpan]) -> Option<&str> {
    take_keyword(&mut input, Keyword::INSERT)?;
    take_keyword(&mut input, Keyword::INTO)?;
    let name = take_name(&mut input)?;
    take_keyword(&mut input, Keyword::VALUES)?;
    separated(&mut input, |input| {
        take_token(input, Token::LParen)?;
        separated(input, take_literal)?;
        take_token(input, Token::RParen)
    })?;
    input.is_empty().then_some(name)
}

fn aggregate_select(mut input: &[TokenWithSpan]) -> Option<&str> {
    take_keyword(&mut input, Keyword::SELECT)?;
    separated(&mut input, |input| {
        let count = take_keyword(input, Keyword::COUNT).is_some();
        if !count {
            let (first, rest) = input.split_first()?;
            if !matches!(&first.token, Token::Word(w) if w.quote_style.is_none() && w.value.eq_ignore_ascii_case("sum"))
            {
                return None;
            }
            *input = rest;
        }
        take_token(input, Token::LParen)?;
        if !count || take_token(input, Token::Mul).is_none() {
            take_name(input)?;
        }
        take_token(input, Token::RParen)?;
        if take_keyword(input, Keyword::AS).is_some() {
            take_name(input)?;
        }
        Some(())
    })?;
    take_keyword(&mut input, Keyword::FROM)?;
    let table = take_name(&mut input)?;
    input.is_empty().then_some(table)
}

#[derive(Serialize)]
pub(super) struct ReplayUnit {
    end: Option<usize>,
    kind: &'static str,
    table: Option<String>,
}

pub(super) fn duckdb_replay_units(sql: &str) -> Option<Vec<ReplayUnit>> {
    let tokens = tokens(sql, DialectKind::DuckDB)?;
    let source = SourceMap::new(sql);
    let mut start = 0;
    let mut units = Vec::new();
    for end in 0..=tokens.len() {
        if end < tokens.len() && tokens[end].token != Token::SemiColon {
            continue;
        }
        if start < end {
            let statement = &tokens[start..end];
            let (kind, table) = if let Some(table) = create_table(statement) {
                ("create", Some(table))
            } else if let Some(table) = insert_values(statement) {
                ("insert", Some(table))
            } else if let Some(table) = aggregate_select(statement) {
                ("query", Some(table))
            } else if scalar_select(statement).is_some() {
                ("query", None)
            } else {
                ("other", None)
            };
            let end = tokens
                .get(end)
                .and_then(|token| source.span(token.span))
                .map(|span| span.end);
            units.push(ReplayUnit {
                end,
                kind,
                table: table.map(str::to_ascii_lowercase),
            });
        }
        start = end + 1;
    }
    Some(units)
}

pub(super) fn duckdb_replay_units_binary(sql: &str) -> Option<Vec<u8>> {
    let units = duckdb_replay_units(sql)?;
    let mut buf = Vec::with_capacity(9 + units.len() * 24);
    buf.extend_from_slice(b"OCCM\x03");
    buf.extend_from_slice(&(units.len() as u32).to_le_bytes());
    for u in &units {
        let end_val: i64 = match u.end {
            Some(e) => e as i64,
            None => -1,
        };
        buf.extend_from_slice(&end_val.to_le_bytes());
        let kind_byte: u8 = match u.kind {
            "create" => 1,
            "insert" => 2,
            "query" => 3,
            _ => 0,
        };
        buf.push(kind_byte);
        match &u.table {
            None => buf.push(0),
            Some(tbl) => {
                buf.push(1);
                buf.extend_from_slice(&(tbl.len() as u32).to_le_bytes());
                buf.extend_from_slice(tbl.as_bytes());
            }
        }
    }
    Some(buf)
}

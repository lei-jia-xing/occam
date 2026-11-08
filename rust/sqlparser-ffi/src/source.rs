use crate::DialectKind;
use sqlparser::dialect::{Dialect, GenericDialect, MySqlDialect, PostgreSqlDialect, SQLiteDialect};
use sqlparser::keywords::Keyword;
use sqlparser::tokenizer::{Location, Span, Token, TokenWithSpan};

#[derive(Clone, Copy)]
pub(super) struct ByteSpan {
    pub(super) start: usize,
    pub(super) end: usize,
}
impl ByteSpan {
    pub(super) fn checked(source: &str, start: usize, end: usize) -> Option<Self> {
        (start < end
            && end <= source.len()
            && source.is_char_boundary(start)
            && source.is_char_boundary(end))
        .then_some(Self { start, end })
    }
}

pub(super) struct SourceMap {
    lines: Vec<Vec<usize>>,
}

impl SourceMap {
    pub(super) fn new(source: &str) -> Self {
        let mut lines = vec![vec![0]];
        for (index, ch) in source.char_indices() {
            let end = index + ch.len_utf8();
            if ch == '\n' {
                lines.push(vec![end]);
            } else {
                lines.last_mut().unwrap().push(end);
            }
        }
        Self { lines }
    }

    pub(super) fn offset(&self, location: Location) -> Option<usize> {
        let line = usize::try_from(location.line).ok()?.checked_sub(1)?;
        let column = usize::try_from(location.column).ok()?.checked_sub(1)?;
        self.lines.get(line)?.get(column).copied()
    }

    pub(super) fn span(&self, span: Span) -> Option<ByteSpan> {
        let start = self.offset(span.start)?;
        let end = self.offset(span.end)?;
        (start <= end).then_some(ByteSpan { start, end })
    }
}

pub(super) fn dialect(kind: DialectKind) -> &'static dyn Dialect {
    match kind {
        DialectKind::Generic => &GenericDialect {},
        DialectKind::PostgreSql => &PostgreSqlDialect {},
        DialectKind::MySql => &MySqlDialect {},
        DialectKind::SQLite => &SQLiteDialect {},
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub(super) enum TokenKind {
    Word(Keyword),
    Semicolon,
    Comma,
    LeftParen,
    RightParen,
    Whitespace,
    Eof,
    Other,
}

#[derive(Clone, Copy)]
pub(super) struct TokenPosition {
    pub(super) span: Span,
    pub(super) start: usize,
    pub(super) end: usize,
    pub(super) kind: TokenKind,
    pub(super) parentheses: Option<ByteSpan>,
}

impl TokenPosition {
    pub(super) fn new(
        token: &TokenWithSpan,
        source_map: Option<&SourceMap>,
    ) -> Result<Self, String> {
        let (start, end) = match source_map {
            Some(source_map) => {
                let span = source_map
                    .span(token.span)
                    .ok_or_else(|| "sqlparser returned an unmappable token span".to_string())?;
                (span.start, span.end)
            }
            None => (0, 0),
        };
        let kind = match &token.token {
            Token::EOF => TokenKind::Eof,
            Token::Word(word) => TokenKind::Word(word.keyword),
            Token::SemiColon => TokenKind::Semicolon,
            Token::Comma => TokenKind::Comma,
            Token::LParen => TokenKind::LeftParen,
            Token::RParen => TokenKind::RightParen,
            Token::Whitespace(_) => TokenKind::Whitespace,
            _ => TokenKind::Other,
        };
        Ok(Self {
            span: token.span,
            start,
            end,
            kind,
            parentheses: None,
        })
    }

    pub(super) fn is_keyword(self, keyword: Keyword) -> bool {
        matches!(self.kind, TokenKind::Word(found) if found == keyword)
    }
}

pub(super) fn token_positions(
    tokens: &[TokenWithSpan],
    source_map: Option<&SourceMap>,
) -> Result<Vec<TokenPosition>, String> {
    let mut positions: Vec<_> = tokens
        .iter()
        .map(|token| TokenPosition::new(token, source_map))
        .collect::<Result<_, _>>()?;
    if source_map.is_none() {
        return Ok(positions);
    }
    let mut openings = Vec::new();
    for index in 0..positions.len() {
        match positions[index].kind {
            TokenKind::LeftParen => openings.push(index),
            TokenKind::RightParen => {
                if let Some(open) = openings.pop() {
                    let pair = ByteSpan {
                        start: positions[open].start,
                        end: positions[index].end,
                    };
                    positions[open].parentheses = Some(pair);
                    positions[index].parentheses = Some(pair);
                }
            }
            TokenKind::Semicolon => openings.clear(),
            _ => {}
        }
    }
    Ok(positions)
}

use crate::DialectKind;
use sqlparser::dialect::{
    ClickHouseDialect, Dialect, DuckDbDialect, MySqlDialect, PostgreSqlDialect, SQLiteDialect,
};
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

/// Maps 1-based `(line, column)` locations into 0-based UTF-8 byte offsets.
pub(super) struct SourceMap {
    source_len: usize,
    line_starts: Vec<usize>,
    non_ascii_lines: Option<Vec<Option<Vec<usize>>>>,
}

impl SourceMap {
    /// Builds a line/column offset index over the provided UTF-8 source string.
    pub(super) fn new(source: &str) -> Self {
        if source.is_ascii() {
            let bytes = source.as_bytes();
            let mut line_starts = Vec::with_capacity(bytes.len() / 32 + 1);
            line_starts.push(0);
            for (index, &byte) in bytes.iter().enumerate() {
                if byte == b'\n' {
                    line_starts.push(index + 1);
                }
            }
            Self {
                source_len: bytes.len(),
                line_starts,
                non_ascii_lines: None,
            }
        } else {
            let mut start = 0;
            let mut line_starts = Vec::with_capacity(source.len() / 32 + 1);
            let mut lines = Vec::with_capacity(line_starts.capacity());
            for line in source.split('\n') {
                line_starts.push(start);
                // ASCII columns are already byte offsets. Build a per-column
                // index only for the lines that actually contain UTF-8.
                lines.push((!line.is_ascii()).then(|| {
                    line.char_indices()
                        .map(|(index, _)| start + index)
                        .chain(std::iter::once(start + line.len()))
                        .collect()
                }));
                start += line.len() + 1;
            }
            Self {
                source_len: source.len(),
                line_starts,
                non_ascii_lines: Some(lines),
            }
        }
    }

    /// Converts a 1-based AST location into a 0-based byte offset.
    pub(super) fn offset(&self, location: Location) -> Option<usize> {
        let line = usize::try_from(location.line).ok()?.checked_sub(1)?;
        let column = usize::try_from(location.column).ok()?.checked_sub(1)?;
        if let Some(Some(offsets)) = self
            .non_ascii_lines
            .as_ref()
            .and_then(|lines| lines.get(line))
        {
            return offsets.get(column).copied();
        }
        let start = *self.line_starts.get(line)?;
        let line_end = match self.line_starts.get(line + 1) {
            Some(&next_start) => next_start - 1,
            None => self.source_len,
        };
        (column <= line_end - start).then(|| start + column)
    }

    /// Converts an AST `Span` into a `ByteSpan`.
    pub(super) fn span(&self, span: Span) -> Option<ByteSpan> {
        let start = self.offset(span.start)?;
        let end = self.offset(span.end)?;
        (start <= end).then_some(ByteSpan { start, end })
    }
}

/// Returns the sqlparser dialect instance for the given [`DialectKind`].
pub(super) fn dialect(kind: DialectKind) -> &'static dyn Dialect {
    match kind {
        DialectKind::PostgreSql => &PostgreSqlDialect {},
        DialectKind::MySql => &MySqlDialect {},
        DialectKind::SQLite => &SQLiteDialect {},
        DialectKind::DuckDB => &DuckDbDialect {},
        DialectKind::ClickHouse => &ClickHouseDialect {},
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

/// Maps token locations into byte spans and resolves matching parenthesis pairs.
pub(super) fn token_positions(
    tokens: &[TokenWithSpan],
    source_map: &SourceMap,
) -> Result<Vec<TokenPosition>, String> {
    let mut positions = Vec::with_capacity(tokens.len());
    for token in tokens {
        positions.push(TokenPosition::new(token, Some(source_map))?);
    }
    let mut openings = Vec::with_capacity(16);
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

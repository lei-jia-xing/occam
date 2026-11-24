use crate::DialectKind;
use crate::ir;
use crate::source::{ByteSpan, SourceMap, TokenPosition};
use sqlparser::tokenizer::Span;

#[derive(Clone)]
pub(crate) struct CteScope {
    pub(crate) recursive: bool,
    pub(crate) names: Vec<String>,
    pub(crate) body_spans: Vec<Option<(usize, usize)>>,
}

/// One lexical query scope for table aliases. `parent` points at the enclosing
/// query scope so correlated references can still resolve outward without
/// leaking aliases between sibling subqueries.
#[derive(Clone)]
pub(crate) struct AliasScope {
    pub(crate) parent: Option<usize>,
    /// Canonical alias name and the canonical table parts it names. `None`
    /// marks an alias whose target is not a directly modelled base table
    /// (derived tables, table functions), which still shadows outer aliases.
    pub(crate) aliases: Vec<(String, Option<Vec<String>>)>,
    /// Number of parent aliases lexically visible through this scope. A
    /// non-LATERAL derived table sees none; a LATERAL one sees the aliases
    /// declared before it. `None` leaves the parent unfiltered.
    pub(crate) parent_alias_limit: Option<usize>,
    /// Number of parent table factors lexically visible through this scope.
    /// This also gates unaliased table-name references, which do not go
    /// through the alias list.
    pub(crate) parent_factor_limit: Option<usize>,
    /// Number of table factors seen directly in this scope.
    pub(crate) factors: usize,
}

/// A relation name encountered while visiting a statement, together with the
/// lexical scope and table-factor position that determine which qualifiers may
/// refer to it.
pub(crate) struct RelationUse {
    pub(crate) name: Option<ir::NameRef>,
    pub(crate) span: Span,
    pub(crate) is_cte: bool,
    pub(crate) scope: usize,
    pub(crate) alias: Option<String>,
    pub(crate) factor: usize,
    /// PostgreSQL `ONLY` on this table factor: partition/inheritance
    /// children are excluded from the read.
    pub(crate) only: bool,
}

pub(crate) enum WholeRowUse {
    Source(Span),
    Projection {
        qualifier: Vec<String>,
        scope: usize,
    },
}

/// Everything needed to analyze one parsed statement against its source unit.
#[derive(Clone, Copy)]
pub(crate) struct AnalysisContext<'a> {
    pub(crate) dialect: DialectKind,
    pub(crate) rules: &'a crate::ir::IdentifierRules,
    pub(crate) source: &'a str,
    pub(crate) source_map: &'a SourceMap,
    pub(crate) tokens: &'a [TokenPosition],
    pub(crate) unit: ByteSpan,
    pub(crate) chunk_edits: bool,
    pub(crate) column_ir: bool,
    pub(crate) syntax_edits: bool,
    pub(crate) syntax_only: bool,
    /// Whether pure built-in functions ([`crate::pure`]) may lift routine fences.
    ///
    /// Disabled (`false`) script-wide whenever any routine DDL statement or parse error
    /// exists in the script, guaranteeing that potential routine shadowing restores blanket fences.
    pub(crate) pure_table_enabled: bool,
}

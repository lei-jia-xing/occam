//! Binary serialization protocol for FFI data exchange between Rust and Occam (Haskell).
//!
//! Encodes [`NativeResponse`] into a compact binary format starting with the `OCCM\x01` magic
//! header, followed by initial session state, parsed statement units, candidate syntactic edits,
//! and atomic column reduction targets.

use crate::columns::{ColumnSite, ColumnSiteKind, ColumnTarget};
use crate::ir::{
    CatalogOp, Ident, Name, NativeFacts, ScopedBarrier, SearchPathEntry, SessionEffect,
    SessionInit, TxEffect,
};
use crate::{NativeEdit, NativeEditCategory, NativeResponse, NativeUnit};

/// 5-byte magic header prefix (`b"OCCM\x01"`) identifying the Occam binary response format version 1.
pub const OCCM_MAGIC: &[u8; 5] = b"OCCM\x01";

/// A byte buffer builder for serializing primitive types and structured payloads in little-endian order.
pub struct BinaryWriter {
    buf: Vec<u8>,
}

impl BinaryWriter {
    /// Creates a new `BinaryWriter` preallocated with initial capacity.
    pub fn new() -> Self {
        Self {
            buf: Vec::with_capacity(4096),
        }
    }

    #[inline]
    pub fn write_u8(&mut self, val: u8) {
        self.buf.push(val);
    }

    #[inline]
    pub fn write_bool(&mut self, val: bool) {
        self.buf.push(if val { 1 } else { 0 });
    }

    #[inline]
    pub fn write_u32(&mut self, val: u32) {
        self.buf.extend_from_slice(&val.to_le_bytes());
    }

    #[inline]
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.write_u32(bytes.len() as u32);
        self.buf.extend_from_slice(bytes);
    }

    #[inline]
    pub fn write_str(&mut self, s: &str) {
        self.write_bytes(s.as_bytes());
    }

    #[inline]
    pub fn write_opt_str(&mut self, opt: Option<&str>) {
        match opt {
            None => self.write_u8(0),
            Some(s) => {
                self.write_u8(1);
                self.write_str(s);
            }
        }
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.buf
    }
}

fn write_ident(w: &mut BinaryWriter, ident: &Ident) {
    w.write_str(&ident.0);
}

fn write_name(w: &mut BinaryWriter, name: &Name) {
    w.write_u32(name.0.len() as u32);
    for ident in &name.0 {
        write_ident(w, ident);
    }
}

fn write_search_path_entry(w: &mut BinaryWriter, entry: &SearchPathEntry) {
    match entry {
        SearchPathEntry::CurrentUser => {
            w.write_u8(0);
        }
        SearchPathEntry::Name { name } => {
            w.write_u8(1);
            write_name(w, name);
        }
    }
}

fn write_session_init(w: &mut BinaryWriter, session: &SessionInit) {
    w.write_u32(session.namespace_prefix.len() as u32);
    for ident in &session.namespace_prefix {
        write_ident(w, ident);
    }

    w.write_u32(session.search_path.len() as u32);
    for entry in &session.search_path {
        write_search_path_entry(w, entry);
    }

    match &session.current_user {
        None => w.write_u8(0),
        Some(user) => {
            w.write_u8(1);
            write_ident(w, user);
        }
    }

    w.write_bool(session.autocommit);
    w.write_bool(session.foreign_key_checks);

    w.write_u32(session.variables.len() as u32);
    for (k, v) in &session.variables {
        w.write_str(k);
        w.write_str(v);
    }
}

fn category_code(cat: NativeEditCategory) -> u8 {
    match cat {
        NativeEditCategory::Expression => 0,
        NativeEditCategory::Clause => 1,
        NativeEditCategory::Constraint => 2,
        NativeEditCategory::List => 3,
        NativeEditCategory::Column => 4,
        NativeEditCategory::Type => 5,
        NativeEditCategory::Literal => 6,
    }
}

fn write_edit(w: &mut BinaryWriter, edit: &NativeEdit) {
    w.write_u32(edit.start as u32);
    w.write_u32(edit.end as u32);
    w.write_str(&edit.replacement);
    w.write_u8(category_code(edit.category));
    w.write_str(&edit.label);
}

#[derive(serde::Serialize)]
struct BinarySemanticFacts<'a> {
    ops: &'a [CatalogOp],
    session: &'a [SessionEffect],
    pre_tx: &'a [TxEffect],
    post_tx: &'a [TxEffect],
    barriers: &'a [ScopedBarrier],
}

fn write_facts(w: &mut BinaryWriter, facts: &NativeFacts) -> Result<(), serde_json::Error> {
    let has_semantics = !facts.ops.is_empty()
        || !facts.session.is_empty()
        || !facts.pre_tx.is_empty()
        || !facts.post_tx.is_empty()
        || !facts.barriers.is_empty();

    if has_semantics {
        let sem = BinarySemanticFacts {
            ops: &facts.ops,
            session: &facts.session,
            pre_tx: &facts.pre_tx,
            post_tx: &facts.post_tx,
            barriers: &facts.barriers,
        };
        let json_bytes = serde_json::to_vec(&sem)?;
        w.write_bytes(&json_bytes);
    } else {
        w.write_u32(0);
    }

    w.write_u32(facts.edits.len() as u32);
    for edit in &facts.edits {
        write_edit(w, edit);
    }
    Ok(())
}

fn write_unit(w: &mut BinaryWriter, unit: &NativeUnit) -> Result<(), serde_json::Error> {
    w.write_u32(unit.start as u32);
    w.write_u32(unit.end as u32);
    w.write_opt_str(unit.parse_error.as_deref());
    match &unit.facts {
        None => w.write_u8(0),
        Some(facts) => {
            w.write_u8(1);
            write_facts(w, facts)?;
        }
    }
    Ok(())
}

fn site_kind_code(kind: ColumnSiteKind) -> u8 {
    match kind {
        ColumnSiteKind::CreateDef => 0,
        ColumnSiteKind::InsertListItem => 1,
        ColumnSiteKind::ValuesItem => 2,
    }
}

fn write_column_site(w: &mut BinaryWriter, site: &ColumnSite) {
    w.write_u32(site.stmt as u32);
    w.write_u32(site.start as u32);
    w.write_u32(site.end as u32);
    w.write_u8(site_kind_code(site.kind));
}

fn write_column_target(w: &mut BinaryWriter, target: &ColumnTarget) {
    write_name(w, &target.table);
    write_ident(w, &target.column);
    w.write_u32(target.ordinal as u32);
    w.write_u32(target.sites.len() as u32);
    for site in &target.sites {
        write_column_site(w, site);
    }
    w.write_opt_str(target.blocked.as_deref());
}

/// Serializes the entire [`NativeResponse`] payload into binary OCCM v1 format.
pub(crate) fn encode_native_response(
    response: &NativeResponse,
) -> Result<Vec<u8>, serde_json::Error> {
    let mut w = BinaryWriter::new();
    w.buf.extend_from_slice(OCCM_MAGIC);
    write_session_init(&mut w, &response.initial_session);
    w.write_u32(response.units.len() as u32);
    for unit in &response.units {
        write_unit(&mut w, unit)?;
    }
    w.write_u32(response.columns.len() as u32);
    for col in &response.columns {
        write_column_target(&mut w, col);
    }
    Ok(w.into_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    #[test]
    fn test_encode_empty_response() {
        let response = NativeResponse {
            initial_session: SessionInit {
                namespace_prefix: Vec::new(),
                search_path: Vec::new(),
                current_user: None,
                autocommit: true,
                foreign_key_checks: true,
                variables: BTreeMap::new(),
            },
            units: Vec::new(),
            columns: Vec::new(),
        };
        let encoded = encode_native_response(&response).expect("encoding succeeds");
        assert!(encoded.starts_with(OCCM_MAGIC));
        assert!(encoded.len() > 5);
    }
}

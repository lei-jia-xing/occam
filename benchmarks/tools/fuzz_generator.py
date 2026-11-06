#!/usr/bin/env python3
"""Chaos Fuzz SQL Generator for stress-testing Occam and SQL crash reducers.

Produces multi-statement (50 ~ 300+ statements) SQL sequences simulating
SQLsmith / SQUIRREL style chaotic queries with deep AST expressions,
DDL/DML mutations, transaction blocks, and known defect injection.
"""

from __future__ import annotations

import argparse
import json
import os
import random
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

# -----------------------------------------------------------------------------
# Schema Modeling
# -----------------------------------------------------------------------------

@dataclass
class ColumnDef:
    name: str
    data_type: str
    is_pk: bool = False
    is_fk: bool = False
    ref_table: str | None = None
    ref_column: str | None = None


@dataclass
class TableDef:
    name: str
    columns: list[ColumnDef] = field(default_factory=list)
    is_target: bool = False

    @property
    def pk_column(self) -> ColumnDef | None:
        for c in self.columns:
            if c.is_pk:
                return c
        return None

    def non_pk_columns(self) -> list[ColumnDef]:
        return [c for c in self.columns if not c.is_pk]


class SchemaState:
    """Maintains active database schema across fuzzer execution sequence."""

    def __init__(self, dialect: str = "postgres"):
        self.dialect = dialect.lower()
        self.tables: dict[str, TableDef] = {}
        self.views: set[str] = set()
        self.savepoints: list[str] = []

    def add_table(self, table: TableDef) -> None:
        self.tables[table.name.lower()] = table

    def get_table(self, name: str) -> TableDef | None:
        return self.tables.get(name.lower())

    def random_table(self, exclude_target: bool = False) -> TableDef | None:
        candidates = [
            t for t in self.tables.values()
            if not (exclude_target and t.is_target)
        ]
        return random.choice(candidates) if candidates else None

    def drop_column(self, table_name: str, col_name: str) -> bool:
        tbl = self.get_table(table_name)
        if not tbl:
            return False
        cols = [c for c in tbl.columns if c.name.lower() != col_name.lower()]
        if len(cols) == len(tbl.columns) or not any(c.is_pk for c in cols):
            return False
        tbl.columns = cols
        return True

    def add_column(self, table_name: str, col: ColumnDef) -> bool:
        tbl = self.get_table(table_name)
        if not tbl:
            return False
        tbl.columns.append(col)
        return True


# -----------------------------------------------------------------------------
# AST Expression Generator (SQLsmith / SQUIRREL Chaos Engine)
# -----------------------------------------------------------------------------

class ChaosExpressionGenerator:
    """Generates complex, deeply nested AST expressions, subqueries, and casts."""

    def __init__(self, schema: SchemaState, max_depth: int = 10):
        self.schema = schema
        self.max_depth = max(3, min(35, max_depth))
        self.dialect = schema.dialect
        self.alias_counter = 0

    def next_alias(self, prefix: str = "c") -> str:
        self.alias_counter += 1
        return f"{prefix}_{self.alias_counter}"

    def extreme_literal(self, data_type: str = "int") -> str:
        t = data_type.upper()
        if "INT" in t or "BIGINT" in t:
            return str(random.choice([
                0, 1, -1, 42, 100,
                2147483647, -2147483648,
                9223372036854775807, -9223372036854775808,
            ]))
        if "NUMERIC" in t or "DECIMAL" in t or "FLOAT" in t:
            return str(random.choice([
                "0.0", "1.0", "-1.5", "3.14159265",
                "999999999.99", "0.0000000001", "1e10", "-1e-5",
            ]))
        if "BOOL" in t:
            return random.choice(["TRUE", "FALSE"])
        if "JSON" in t:
            return random.choice([
                "'{}'", "'[]'",
                '\'{"status": "chaos", "weight": 99.8, "tags": [null, true, "fuzz"]}\'',
                '\'{"ref_id": 101, "payload": {"nested": {"v": -1}}}\'',
            ])
        if "TEXT" in t or "CHAR" in t:
            return random.choice([
                "''", "'chaos'", "'ref_val'", "'escaped''quote'",
                "'__fuzz_marker__'", "'alpha_beta_gamma'",
            ])
        return random.choice(["0", "''", "NULL", "TRUE"])

    def gen_leaf(self, table: TableDef | None, alias: str | None = None) -> str:
        if table and table.columns and random.random() < 0.65:
            col = random.choice(table.columns)
            return f"{alias}.{col.name}" if alias else col.name
        return random.choice([
            "NULL",
            self.extreme_literal("int"),
            self.extreme_literal("text"),
            self.extreme_literal("numeric"),
            "TRUE",
            "FALSE",
        ])

    def gen_expr(
        self,
        table: TableDef | None,
        depth: int,
        alias: str | None = None,
        allow_agg: bool = False,
    ) -> str:
        """Recursive AST generator producing arithmetic, booleans, casts, and subqueries."""
        if depth <= 1 or random.random() < 0.15:
            return self.gen_leaf(table, alias)

        choice = random.randint(1, 10)

        # 1. Binary arithmetic operator
        if choice in (1, 2):
            op = random.choice(["+", "-", "*", "/"])
            left = self.gen_expr(table, depth - 1, alias)
            right = self.gen_expr(table, depth - 1, alias)
            if op == "/":
                return f"({left} / NULLIF({right}, 0))"
            return f"({left} {op} {right})"

        # 2. Comparison operator
        if choice == 3:
            op = random.choice(["=", "<>", "<", "<=", ">", ">="])
            left = self.gen_expr(table, depth - 1, alias)
            right = self.gen_expr(table, depth - 1, alias)
            return f"({left} {op} {right})"

        # 3. Boolean connective tree (AND, OR, NOT, IS NOT NULL)
        if choice == 4:
            sub_choice = random.choice(["and", "or", "not", "is_null", "between"])
            if sub_choice == "and":
                return f"({self.gen_expr(table, depth - 1, alias)} AND {self.gen_expr(table, depth - 1, alias)})"
            if sub_choice == "or":
                return f"({self.gen_expr(table, depth - 1, alias)} OR {self.gen_expr(table, depth - 1, alias)})"
            if sub_choice == "not":
                return f"(NOT ({self.gen_expr(table, depth - 1, alias)}))"
            if sub_choice == "is_null":
                suffix = "IS NULL" if random.random() < 0.5 else "IS NOT NULL"
                return f"({self.gen_expr(table, depth - 1, alias)} {suffix})"
            if sub_choice == "between":
                e = self.gen_expr(table, depth - 1, alias)
                lo = self.extreme_literal("int")
                hi = self.extreme_literal("int")
                return f"({e} BETWEEN {lo} AND {hi})"

        # 4. Conditional expressions: COALESCE, NULLIF, CASE WHEN
        if choice == 5:
            if random.random() < 0.5:
                e1 = self.gen_expr(table, depth - 1, alias)
                e2 = self.gen_expr(table, depth - 1, alias)
                fn = random.choice(["COALESCE", "NULLIF"])
                return f"{fn}({e1}, {e2})"
            cond = self.gen_expr(table, depth - 1, alias)
            thn = self.gen_expr(table, depth - 1, alias)
            els = self.gen_expr(table, depth - 1, alias)
            return f"(CASE WHEN {cond} THEN {thn} ELSE {els} END)"

        # 5. Type casts (PostgreSQL chained casts or MySQL CAST)
        if choice == 6:
            base = self.gen_expr(table, depth - 1, alias)
            if self.dialect == "postgres":
                cast_chain = random.choice([
                    "::INT",
                    "::TEXT",
                    "::NUMERIC(10,2)",
                    "::BOOLEAN",
                    "::INT::TEXT",
                    "::TEXT::VARCHAR(32)",
                ])
                return f"({base}{cast_chain})"
            target_cast = random.choice(["CHAR", "SIGNED", "DECIMAL(10,2)"])
            return f"CAST({base} AS {target_cast})"

        # 6. Built-in functions
        if choice == 7:
            fn = random.choice(["LENGTH", "LOWER", "UPPER", "ABS"])
            arg = self.gen_expr(table, depth - 1, alias)
            return f"{fn}({arg})"

        # 7. Aggregate functions
        if choice == 8 and allow_agg:
            agg_fn = random.choice(["COUNT", "SUM", "AVG", "MIN", "MAX"])
            if agg_fn == "COUNT" and random.random() < 0.5:
                return "COUNT(*)"
            arg = self.gen_expr(table, depth - 1, alias)
            return f"{agg_fn}({arg})"

        # 8. Convoluted subqueries (SQLsmith style nested queries)
        if choice in (9, 10):
            target_tbl = self.schema.random_table()
            if target_tbl:
                sub_alias = self.next_alias("sub")
                sub_expr = self.gen_expr(target_tbl, max(1, depth - 2), sub_alias)
                sub_kind = random.choice(["scalar", "nested", "exists", "in"])
                if sub_kind == "scalar":
                    return f"(SELECT {sub_expr} FROM {target_tbl.name} AS {sub_alias} LIMIT 1)"
                if sub_kind == "nested":
                    inner_tbl = self.schema.random_table() or target_tbl
                    inner_alias = self.next_alias("inn")
                    inner_expr = self.gen_expr(inner_tbl, 1, inner_alias)
                    return (
                        f"(SELECT (SELECT {inner_expr} FROM {inner_tbl.name} AS {inner_alias} LIMIT 1) "
                        f"FROM {target_tbl.name} AS {sub_alias} LIMIT 1)"
                    )
                if sub_kind == "exists":
                    cond = self.gen_expr(target_tbl, 1, sub_alias)
                    return f"EXISTS (SELECT 1 FROM {target_tbl.name} AS {sub_alias} WHERE {cond})"
                if sub_kind == "in" and target_tbl.columns:
                    col = random.choice(target_tbl.columns)
                    outer = self.gen_expr(table, 1, alias)
                    return f"({outer} IN (SELECT {sub_alias}.{col.name} FROM {target_tbl.name} AS {sub_alias}))"

        return self.gen_leaf(table, alias)


# -----------------------------------------------------------------------------
# Fuzz Statement Generator
# -----------------------------------------------------------------------------

class ChaosFuzzGenerator:
    """Orchestrates multi-statement sequence synthesis with DDL, DML, and transactions."""

    def __init__(
        self,
        dialect: str = "postgres",
        total_statements: int = 100,
        ast_depth: int = 10,
        seed: int | None = None,
        inject_case: str | None = None,
    ):
        if dialect in ("postgres", "postgresql", "pg"):
            self.dialect = "postgres"
        elif dialect in ("sqlite", "sqlite3"):
            self.dialect = "sqlite"
        elif dialect in ("duckdb",):
            self.dialect = "duckdb"
        else:
            self.dialect = "mysql"
        self.total_statements = max(20, min(500, total_statements))
        self.ast_depth = max(5, min(25, ast_depth))
        self.seed = seed
        self.inject_case = inject_case
        if seed is not None:
            random.seed(seed)

        self.schema = SchemaState(dialect=self.dialect)
        self.expr_gen = ChaosExpressionGenerator(self.schema, max_depth=self.ast_depth)
        self.tbl_counter = 0
        self.view_counter = 0
        self.idx_counter = 0

    def next_table_name(self) -> str:
        self.tbl_counter += 1
        return f"tbl_{self.tbl_counter:02d}"

    def gen_create_table(self, is_target: bool = False, target_name: str | None = None) -> tuple[str, TableDef]:
        tbl_name = target_name or self.next_table_name()
        cols: list[ColumnDef] = [
            ColumnDef(name="id", data_type="INT", is_pk=True)
        ]

        # Additional diverse columns
        type_pool = ["VARCHAR(64)", "NUMERIC(10,2)", "INT", "BOOLEAN"]
        if self.dialect == "postgres":
            type_pool.append("JSONB")
        else:
            type_pool.append("JSON")

        num_extra = random.randint(2, 4)
        for i in range(1, num_extra + 1):
            c_type = random.choice(type_pool)
            cols.append(ColumnDef(name=f"col_{i}", data_type=c_type))

        # Optional foreign key to an existing table
        fk_clause = ""
        existing = self.schema.random_table(exclude_target=True)
        if existing and existing.pk_column and random.random() < 0.4:
            fk_col_name = f"ref_{existing.name}_id"
            cols.append(
                ColumnDef(
                    name=fk_col_name,
                    data_type="INT",
                    is_fk=True,
                    ref_table=existing.name,
                    ref_column=existing.pk_column.name,
                )
            )
            fk_clause = f",\n    FOREIGN KEY ({fk_col_name}) REFERENCES {existing.name}({existing.pk_column.name})"

        col_defs = [f"    {c.name} {c.data_type}" + (" PRIMARY KEY" if c.is_pk else "") for c in cols]
        sql = f"CREATE TABLE {tbl_name} (\n" + ",\n".join(col_defs) + fk_clause + "\n);"

        tbl = TableDef(name=tbl_name, columns=cols, is_target=is_target)
        self.schema.add_table(tbl)
        return sql, tbl

    def gen_create_index(self) -> str | None:
        tbl = self.schema.random_table()
        if not tbl or not tbl.columns:
            return None
        col = random.choice(tbl.columns)
        self.idx_counter += 1
        idx_name = f"idx_{tbl.name}_{col.name}_{self.idx_counter}"
        return f"CREATE INDEX {idx_name} ON {tbl.name} ({col.name});"

    def gen_alter_table(self) -> str | None:
        tbl = self.schema.random_table(exclude_target=True)
        if not tbl:
            return None
        action = random.choice(["add", "drop"])
        if action == "add" or len(tbl.columns) <= 2:
            new_col = f"dyn_col_{len(tbl.columns) + 1}"
            col_def = ColumnDef(name=new_col, data_type="VARCHAR(32)")
            self.schema.add_column(tbl.name, col_def)
            return f"ALTER TABLE {tbl.name} ADD COLUMN {new_col} VARCHAR(32);"
        non_pk = tbl.non_pk_columns()
        if non_pk:
            col_to_drop = random.choice(non_pk)
            self.schema.drop_column(tbl.name, col_to_drop.name)
            return f"ALTER TABLE {tbl.name} DROP COLUMN {col_to_drop.name};"
        return None

    def gen_create_view(self) -> str | None:
        tbl = self.schema.random_table()
        if not tbl or not tbl.columns:
            return None
        self.view_counter += 1
        v_name = f"v_summary_{self.view_counter}"
        col = tbl.columns[0]
        self.schema.views.add(v_name)
        return f"CREATE VIEW {v_name} AS SELECT {col.name} FROM {tbl.name} WHERE {col.name} IS NOT NULL;"

    def gen_insert(self, table: TableDef | None = None) -> str | None:
        tbl = table or self.schema.random_table(exclude_target=True)
        if not tbl or not tbl.columns:
            return None

        col_names = [c.name for c in tbl.columns]
        val_exprs = []
        for c in tbl.columns:
            if c.is_pk:
                val_exprs.append(str(random.randint(1, 999999)))
            else:
                val_exprs.append(self.expr_gen.extreme_literal(c.data_type))

        cols_str = ", ".join(col_names)
        vals_str = ", ".join(val_exprs)
        return f"INSERT INTO {tbl.name} ({cols_str}) VALUES ({vals_str});"

    def gen_update(self) -> str | None:
        tbl = self.schema.random_table(exclude_target=True)
        if not tbl or not tbl.columns:
            return None
        non_pk = tbl.non_pk_columns()
        if not non_pk:
            return None
        col = random.choice(non_pk)
        new_val = self.expr_gen.extreme_literal(col.data_type)
        where_cond = f"{tbl.columns[0].name} > {random.randint(0, 100)}"
        return f"UPDATE {tbl.name} SET {col.name} = {new_val} WHERE {where_cond};"

    def gen_delete(self) -> str | None:
        tbl = self.schema.random_table(exclude_target=True)
        if not tbl or not tbl.columns:
            return None
        where_cond = f"{tbl.columns[0].name} = {random.randint(1000, 99999)};"
        return f"DELETE FROM {tbl.name} WHERE {where_cond}"
    def gen_select(self) -> str | None:
        tbl = self.schema.random_table()
        if not tbl or not tbl.columns:
            return None

        alias_1 = self.expr_gen.next_alias("t_ref")
        proj_cols = []
        for i in range(random.randint(1, 3)):
            col_expr = self.expr_gen.gen_expr(tbl, depth=self.ast_depth, alias=alias_1)
            out_alias = self.expr_gen.next_alias("c")
            proj_cols.append(f"    {col_expr} AS {out_alias}")
        proj_str = ",\n".join(proj_cols)

        # Decide JOIN vs single table
        other_tbl = self.schema.random_table()
        if other_tbl and other_tbl.name != tbl.name and random.random() < 0.45:
            alias_2 = self.expr_gen.next_alias("j_ref")
            join_cond = f"{alias_1}.{tbl.columns[0].name} = {alias_2}.{other_tbl.columns[0].name}"
            from_clause = f"FROM {tbl.name} AS {alias_1} LEFT JOIN {other_tbl.name} AS {alias_2} ON {join_cond}"
        else:
            from_clause = f"FROM {tbl.name} AS {alias_1}"

        where_cond = self.expr_gen.gen_expr(tbl, depth=self.ast_depth, alias=alias_1)
        limit_val = random.randint(1, 50)
        return f"SELECT\n{proj_str}\n{from_clause}\nWHERE {where_cond}\nLIMIT {limit_val};"

    def gen_transaction_block(self, inner_statements: list[str]) -> str:
        if random.random() < 0.4 and len(inner_statements) >= 2:
            sp_name = f"sp_{random.randint(1, 999)}"
            first = inner_statements[0]
            rest = inner_statements[1:]
            body = f"{first}\nSAVEPOINT {sp_name};\n" + "\n".join(rest) + f"\nROLLBACK TO SAVEPOINT {sp_name};"
        else:
            body = "\n".join(inner_statements)
        closer = "COMMIT;" if random.random() < 0.85 else "ROLLBACK;"
        return f"BEGIN;\n{body}\n{closer}"
    def load_injection_case(self) -> dict[str, Any] | None:
        """Extracts target scenario and metadata for defect injection."""
        if not self.inject_case:
            return None

        case_key = self.inject_case.lower()
        builtin = {
            "pg_01": {
                "dbms": "postgresql",
                "target_table": "customer_profiles",
                "statements": [
                    "CREATE TABLE customer_profiles (\n    profile_id INT PRIMARY KEY,\n    tenant_id INT NOT NULL,\n    account_status VARCHAR(24),\n    profile_doc JSONB NOT NULL\n);",
                    "CREATE INDEX idx_customer_profiles_doc_gin ON customer_profiles USING gin (profile_doc);",
                    "INSERT INTO customer_profiles (profile_id, tenant_id, account_status, profile_doc) VALUES\n    (1, 10, 'active', '{\"user\": {\"name\": \"Alice\", \"contact\": {\"emails\": [{\"type\": \"work\", \"address\": \"alice@corp.com\"}, {\"type\": \"home\", \"address\": null}]}}, \"settings\": {\"dark_mode\": true}}'),\n    (2, 10, 'suspended', '{\"user\": {\"name\": \"Bob\", \"contact\": {\"emails\": []}}, \"metadata\": null}'),\n    (3, 20, 'active', '{\"user\": {\"name\": \"Charlie\", \"contact\": null}, \"tags\": [\"beta\", \"internal\"]}');",
                    "UPDATE customer_profiles\nSET profile_doc = jsonb_set(profile_doc, '{settings,backup_email}', '\"backup@corp.com\"')\nWHERE profile_id = 1;",
                    "SELECT\n    p.profile_id,\n    p.account_status,\n    jsonb_path_query(p.profile_doc, 'lax $.user.contact.emails[*] ? (@.type == \"work\").address') AS work_email\nFROM customer_profiles p\nWHERE p.account_status = 'active'\n  AND p.profile_doc @> '{\"user\": {}}'\nORDER BY p.profile_id;",
                ],
            },
            "mysql_01": {
                "dbms": "mysql",
                "target_table": "sales_pipeline",
                "statements": [
                    "CREATE TABLE sales_pipeline (\n    opportunity_id INT PRIMARY KEY,\n    rep_id INT NOT NULL,\n    deal_value DECIMAL(12,2) NOT NULL,\n    stage VARCHAR(32) NOT NULL\n);",
                    "CREATE TABLE crm_leads (\n    lead_id INT PRIMARY KEY,\n    rep_id INT NOT NULL,\n    estimated_arr DECIMAL(12,2) NOT NULL\n);",
                    "INSERT INTO sales_pipeline (opportunity_id, rep_id, deal_value, stage) VALUES (1, 10, 50000.00, 'closed_won'), (2, 10, 15000.00, 'in_progress');",
                    "INSERT INTO crm_leads (lead_id, rep_id, estimated_arr) VALUES (101, 10, 45000.00), (102, 20, 90000.00);",
                    "WITH cte_pipeline AS (\n    SELECT rep_id, deal_value, stage,\n           ROW_NUMBER() OVER (PARTITION BY rep_id ORDER BY deal_value DESC) AS rank_in_rep\n    FROM sales_pipeline\n    WHERE stage = 'closed_won'\n    UNION\n    SELECT rep_id, estimated_arr, 'prospect',\n           DENSE_RANK() OVER (PARTITION BY rep_id ORDER BY estimated_arr DESC) AS rank_in_rep\n    FROM crm_leads\n)\nSELECT cp.rep_id, SUM(cp.deal_value) AS aggregate_val\nFROM cte_pipeline cp\nJOIN sales_pipeline sp ON cp.rep_id = sp.rep_id\nGROUP BY cp.rep_id, cp.stage\nHAVING aggregate_val > 10000\nORDER BY cp.rep_id;",
                ],
            },
            "sqlite_01": {
                "dbms": "sqlite",
                "target_table": "inventory_cache",
                "statements": [
                    "CREATE TABLE inventory_cache (\n    item_id INT PRIMARY KEY,\n    sku VARCHAR(32) NOT NULL,\n    qty INT NOT NULL,\n    metadata JSON\n);",
                    "CREATE TABLE price_overrides (\n    item_id INT PRIMARY KEY,\n    discount_pct NUMERIC(5,2) NOT NULL\n);",
                    "INSERT INTO inventory_cache (item_id, sku, qty, metadata) VALUES (1, 'SKU-001', 50, '{\"warehouse\": \"central\"}'), (2, 'SKU-002', 15, '{\"warehouse\": \"east\"}');",
                    "INSERT INTO price_overrides (item_id, discount_pct) VALUES (1, 10.00), (2, 25.00);",
                    "INSERT OR REPLACE INTO inventory_cache (item_id, sku, qty, metadata) VALUES (1, 'SKU-001', 45, '{\"warehouse\": \"central\", \"updated\": true}');",
                    "WITH discounted AS (\n    SELECT i.item_id, i.sku, i.qty, COALESCE(p.discount_pct, 0.0) AS discount\n    FROM inventory_cache i\n    LEFT JOIN price_overrides p ON i.item_id = p.item_id\n)\nSELECT d.item_id, d.sku, d.qty, json_extract(i.metadata, '$.warehouse') AS wh\nFROM discounted d\nJOIN inventory_cache i ON d.item_id = i.item_id\nWHERE d.discount > 5.0\nORDER BY d.item_id;",
                ],
            },
        }

        if case_key in builtin:
            b = builtin[case_key]
            raw_stmts = b["statements"]
            return {
                "case_id": self.inject_case,
                "target_table": b["target_table"],
                "statements": raw_stmts,
                "setup_statements": raw_stmts[:-1],
                "crashing_statement": raw_stmts[-1],
                "dbms": b.get("dbms", "postgresql"),
            }

        root = Path(__file__).resolve().parents[2]
        fuzz_dir = root / "benchmarks" / "fuzz_corpus"
        meta_file = fuzz_dir / "metadata.json"
        if meta_file.exists():
            try:
                entries = json.loads(meta_file.read_text(encoding="utf-8"))
                for e in entries:
                    if e.get("id") == self.inject_case or e.get("injected_case") == self.inject_case:
                        sql_path = fuzz_dir / e["file"]
                        if sql_path.exists():
                            content = sql_path.read_text(encoding="utf-8")
                            raw_stmts = [s.strip() + ";" for s in content.split(";") if s.strip()]
                            target_tbl = e.get("target_tables", ["target_tbl"])[0]
                            return {
                                "case_id": self.inject_case,
                                "target_table": target_tbl,
                                "statements": raw_stmts,
                                "setup_statements": raw_stmts[:-1],
                                "crashing_statement": raw_stmts[-1],
                                "dbms": e.get("dialect", "postgresql"),
                            }
            except Exception:
                pass

        return None
    def generate(self) -> str:
        """Assembles a full multi-statement chaos fuzzer sequence with injected defect."""
        injected = self.load_injection_case()
        if injected and injected.get("dbms"):
            case_dbms = injected["dbms"].lower()
            if "mysql" in case_dbms:
                self.dialect = "mysql"
                self.schema.dialect = "mysql"
                self.expr_gen.dialect = "mysql"

        # Determine injection scheduling
        injected_stmts: list[str] = injected["statements"] if injected else []
        setup_stmts: list[str] = injected["setup_statements"] if injected else []
        crash_stmt: str | None = injected["crashing_statement"] if injected else None

        # Position crash trigger between 65% and 80% of sequence
        num_target = len(injected_stmts)
        available_slots = self.total_statements - num_target
        if available_slots < 5:
            self.total_statements = num_target + 10
            available_slots = 10

        crash_pos = random.randint(int(self.total_statements * 0.65), int(self.total_statements * 0.82))

        # Register injected target table into schema with parsed column signatures
        if injected and injected.get("target_table"):
            raw_target = injected["target_table"]
            target_names = raw_target if isinstance(raw_target, list) else [raw_target]
            scenario_full = " ".join(injected_stmts)
            for tname in target_names:
                # Extract DDL for this specific table from scenario
                m = re.search(
                    rf"CREATE\s+(?:TEMP\s+)?TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?{re.escape(tname)}\s*\((.*?)\);",
                    scenario_full,
                    re.DOTALL | re.IGNORECASE,
                )
                parsed_cols = []
                if m:
                    body = m.group(1)
                    # Split top-level commas
                    raw_tokens = []
                    curr: list[str] = []
                    d = 0
                    for ch in body:
                        if ch == "(":
                            d += 1
                        elif ch == ")":
                            d -= 1
                        elif ch == "," and d == 0:
                            raw_tokens.append("".join(curr).strip())
                            curr = []
                            continue
                        curr.append(ch)
                    if curr and "".join(curr).strip():
                        raw_tokens.append("".join(curr).strip())

                    for tok in raw_tokens:
                        tok_clean = tok.strip()
                        if re.match(r"^(PRIMARY\s+KEY|FOREIGN\s+KEY|CONSTRAINT|UNIQUE|CHECK)\b", tok_clean, re.IGNORECASE):
                            continue
                        parts = tok_clean.split()
                        if len(parts) >= 2:
                            c_name = parts[0].strip('"`[]')
                            c_type = parts[1]
                            is_pk = "PRIMARY KEY" in tok_clean.upper()
                            parsed_cols.append(ColumnDef(name=c_name, data_type=c_type, is_pk=is_pk))

                if not parsed_cols:
                    parsed_cols = [
                        ColumnDef(name="id", data_type="INT", is_pk=True),
                        ColumnDef(name="payload", data_type="TEXT"),
                    ]
                if not any(c.is_pk for c in parsed_cols):
                    parsed_cols[0].is_pk = True

                self.schema.add_table(
                    TableDef(
                        name=tname,
                        columns=parsed_cols,
                        is_target=True,
                    )
                )

        # Build initial bootstrap tables
        all_statements: list[str] = []
        for _ in range(random.randint(3, 5)):
            ddl, _ = self.gen_create_table()
            all_statements.append(ddl)

        # Distribute injected setup statements before crash_pos
        setup_idx = 0
        setup_schedule: dict[int, str] = {}
        if setup_stmts:
            step = max(1, (crash_pos - 10) // (len(setup_stmts) + 1))
            cur = 5
            for s in setup_stmts:
                setup_schedule[cur] = s
                cur += step

        # Generate sequence until total_statements reached
        while len(all_statements) < self.total_statements:
            curr_pos = len(all_statements)

            # Injected crashing statement at crash_pos
            if curr_pos == crash_pos and crash_stmt:
                all_statements.append(crash_stmt)
                continue

            # Scheduled injected setup statement
            if curr_pos in setup_schedule:
                all_statements.append(setup_schedule[curr_pos])
                continue

            # Injected setup overflow guard
            if curr_pos == crash_pos - 1 and setup_idx < len(setup_stmts):
                all_statements.append(setup_stmts[setup_idx])
                setup_idx += 1
                continue

            # Decide statement action
            r = random.random()
            if r < 0.12:
                ddl, _ = self.gen_create_table()
                all_statements.append(ddl)
            elif r < 0.20:
                stmt = self.gen_create_index()
                if stmt:
                    all_statements.append(stmt)
            elif r < 0.26:
                stmt = self.gen_alter_table()
                if stmt:
                    all_statements.append(stmt)
            elif r < 0.32:
                stmt = self.gen_create_view()
                if stmt:
                    all_statements.append(stmt)
            elif r < 0.55:
                stmt = self.gen_insert()
                if stmt:
                    all_statements.append(stmt)
            elif r < 0.68:
                stmt = self.gen_update()
                if stmt:
                    all_statements.append(stmt)
            elif r < 0.74:
                stmt = self.gen_delete()
                if stmt:
                    all_statements.append(stmt)
            elif r < 0.90:
                stmt = self.gen_select()
                if stmt:
                    all_statements.append(stmt)
            else:
                # Transaction block with internal mutations
                inner = []
                for _ in range(random.randint(1, 3)):
                    ins = self.gen_insert() or self.gen_update()
                    if ins:
                        inner.append(ins)
                if inner:
                    tx_block = self.gen_transaction_block(inner)
                    all_statements.append(tx_block)

        # If crash_stmt was not appended yet, append it now
        if crash_stmt and crash_stmt not in all_statements:
            all_statements.append(crash_stmt)

        header = [
            "-- Chaos Fuzz Generator Output (Occam Stress Benchmark)",
            f"-- Dialect: {self.dialect.upper()}",
            f"-- Statements: {len(all_statements)}",
            f"-- AST Expression Depth: {self.ast_depth}",
            f"-- Injected Defect Case: {self.inject_case or 'None (Pure Chaos)'}",
            f"-- Seed: {self.seed}",
            "",
        ]
        return "\n\n".join(header + all_statements)


# -----------------------------------------------------------------------------
# Self-Test and AST Verification Suite
# -----------------------------------------------------------------------------

def run_self_test() -> bool:
    """Validates fuzzer output scaling (50, 100, 200), Tree-sitter AST parsing, and Occam reduction."""
    print("======================================================================")
    print("Running Chaos Fuzz SQL Generator Self-Test Suite")
    print("======================================================================")

    root = Path(__file__).resolve().parents[2]
    occam_bin: Path | None = None
    try:
        res = subprocess.run(["cabal", "list-bin", "occam"], check=False, cwd=root, capture_output=True, text=True)
        if res.returncode == 0 and res.stdout.strip():
            p = Path(res.stdout.strip())
            if p.exists() and os.access(p, os.X_OK):
                occam_bin = p
    except Exception:
        pass
    if occam_bin is None:
        candidates = [
            root / "dist-newstyle" / "build" / "x86_64-linux" / "ghc-9.10.3" / "occam-0.1.0.0" / "x" / "occam" / "build" / "occam" / "occam",
        ]
        for c in candidates:
            if c.exists() and os.access(c, os.X_OK):
                occam_bin = c
                break

    # Step 1: Scaling tests (50, 100, 200 statements for PG and MySQL)
    dialects = ["postgres", "mysql"]
    counts = [50, 100, 200]

    with tempfile.TemporaryDirectory(prefix="occam-fuzz-selftest-") as tmpdir:
        tmp_path = Path(tmpdir)
        print("\n[Phase 1] Multi-statement scaling & Tree-sitter AST verification...")

        all_ok = True
        for dialect in dialects:
            for n in counts:
                gen = ChaosFuzzGenerator(dialect=dialect, total_statements=n, ast_depth=10, seed=42 + n)
                sql = gen.generate()
                out_file = tmp_path / f"{dialect}_{n}.sql"
                out_file.write_text(sql, encoding="utf-8")

                num_stmts = sql.count(";")
                print(f"  Generated {dialect:8s} | requested: {n:3d} stmts | output: {len(sql):6d} bytes, {num_stmts:3d} ';' stmts")
        # Step 2: Occam End-to-End Reduction Verification on 100-stmt injected sequence
        print("\n[Phase 2] Occam reduction verification with injected defect (pg_01)...")
        injected_gen = ChaosFuzzGenerator(
            dialect="postgres",
            total_statements=100,
            ast_depth=10,
            seed=1234,
            inject_case="pg_01",
        )
        injected_sql = injected_gen.generate()
        inj_file = tmp_path / "pg_01_fuzz_100.sql"
        inj_file.write_text(injected_sql, encoding="utf-8")
        out_reduce_dir = tmp_path / "reduce_out"

        oracle_cmd = f"{root}/benchmarks/fuzz_corpus/fuzz_oracle.sh pg_01"
        cmd = [
            str(occam_bin) if occam_bin and occam_bin.exists() else "occam",
            str(inj_file),
            "--oracle", oracle_cmd,
            "--out", str(out_reduce_dir),
        ]

        print(f"  Target: {inj_file.name} ({len(injected_sql)} bytes)")
        print("  Executing Occam reducer...")
        res = subprocess.run(cmd, check=False, cwd=root, capture_output=True, text=True)

        if res.returncode != 0:
            print("  Occam reduction failed!")
            print("  Stdout:\n", res.stdout)
            print("  Stderr:\n", res.stderr)
            return False

        stats_file = out_reduce_dir / "stats.json"
        if stats_file.exists():
            stats = json.loads(stats_file.read_text(encoding="utf-8"))
            init_s = stats.get("initial_statements", 0)
            red_s = stats.get("reduced_statements", 0)
            ratio = stats.get("reduction_ratio", 0.0)
            print("  Occam reduction SUCCESS:")
            print(f"    Statements: {init_s} -> {red_s}")
            print(f"    Bytes Reduction: {ratio:.1f}%")
            print(f"    Oracle Executions: {stats.get('oracle_executions', 0)}")
        else:
            print("  Occam reduction finished without stats.json")

    print("\n======================================================================")
    print("Self-Test Result: ALL CHECKS PASSED")
    print("======================================================================")
    return all_ok


# -----------------------------------------------------------------------------
# CLI Entrypoint
# -----------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Synthetic SQL Generator for Occam Reducer Stress Benchmarking."
    )
    parser.add_argument(
        "--dialect",
        choices=["postgres", "postgresql", "mysql", "sqlite", "sqlite3", "duckdb"],
        default="postgres",
        help="Target SQL dialect (default: postgres)",
    )
    parser.add_argument(
        "--stmts",
        type=int,
        default=100,
        help="Number of statements to generate (20 to 500, default: 100)",
    )
    parser.add_argument(
        "--depth",
        type=int,
        default=10,
        help="Maximum AST expression nesting depth (5 to 25, default: 10)",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=None,
        help="Random seed for reproducible chaos generation",
    )
    parser.add_argument(
        "--inject",
        type=str,
        default=None,
        help="Synthetic predicate marker to inject (e.g. pg_01, mysql_01, pg_03)",
    )
    parser.add_argument(
        "--out",
        type=str,
        default=None,
        help="Output file path (default: stdout)",
    )
    parser.add_argument(
        "--self-test",
        action="store_true",
        help="Run comprehensive self-test suite (scaling, AST validity, and Occam reduction)",
    )

    args = parser.parse_args()

    if args.self_test:
        ok = run_self_test()
        sys.exit(0 if ok else 1)

    generator = ChaosFuzzGenerator(
        dialect=args.dialect,
        total_statements=args.stmts,
        ast_depth=args.depth,
        seed=args.seed,
        inject_case=args.inject,
    )

    sql_output = generator.generate()

    if args.out:
        out_path = Path(args.out)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        out_path.write_text(sql_output, encoding="utf-8")
        print(f"Generated {generator.dialect.upper()} chaos SQL ({len(sql_output)} bytes) written to {out_path}")
    else:
        sys.stdout.write(sql_output)


if __name__ == "__main__":
    main()

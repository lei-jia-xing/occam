#include "occam_ts.h"
#include <stdlib.h>

extern const TSLanguage *tree_sitter_sql(void);

const TSLanguage *occam_sql_language(void) {
    return tree_sitter_sql();
}

TSParser *occam_parser_new(void) {
    TSParser *parser = ts_parser_new();
    if (!parser) return NULL;
    if (!ts_parser_set_language(parser, occam_sql_language())) {
        ts_parser_delete(parser);
        return NULL;
    }
    return parser;
}

void occam_parser_free(TSParser *parser) {
    if (parser) {
        ts_parser_delete(parser);
    }
}

TSTree *occam_parse_sql(TSParser *parser, const char *sql, uint32_t len) {
    if (!parser || !sql) return NULL;
    return ts_parser_parse_string(parser, NULL, sql, len);
}

void occam_tree_free(TSTree *tree) {
    if (tree) {
        ts_tree_delete(tree);
    }
}

void occam_tree_root_p(const TSTree *tree, TSNode *out) {
    if (!tree || !out) return;
    *out = ts_tree_root_node(tree);
}

const char *occam_node_type_p(const TSNode *node) {
    if (!node) return "";
    return ts_node_type(*node);
}

uint32_t occam_node_start_byte_p(const TSNode *node) {
    if (!node) return 0;
    return ts_node_start_byte(*node);
}

uint32_t occam_node_end_byte_p(const TSNode *node) {
    if (!node) return 0;
    return ts_node_end_byte(*node);
}

uint32_t occam_node_child_count_p(const TSNode *node) {
    if (!node) return 0;
    return ts_node_child_count(*node);
}

void occam_node_child_p(const TSNode *node, uint32_t index, TSNode *out) {
    if (!node || !out) return;
    *out = ts_node_child(*node, index);
}

bool occam_node_is_named_p(const TSNode *node) {
    if (!node) return false;
    return ts_node_is_named(*node);
}

bool occam_node_is_null_p(const TSNode *node) {
    if (!node) return true;
    return ts_node_is_null(*node);
}

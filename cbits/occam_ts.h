#ifndef OCCAM_TS_H
#define OCCAM_TS_H

#include <stdint.h>
#include <stdbool.h>
#include <tree_sitter/api.h>

#ifdef __cplusplus
extern "C" {
#endif

const TSLanguage *occam_sql_language(void);

TSParser *occam_parser_new(void);
void occam_parser_free(TSParser *parser);

TSTree *occam_parse_sql(TSParser *parser, const char *sql, uint32_t len);
void occam_tree_free(TSTree *tree);

void occam_tree_root_p(const TSTree *tree, TSNode *out);
const char *occam_node_type_p(const TSNode *node);
uint32_t occam_node_start_byte_p(const TSNode *node);
uint32_t occam_node_end_byte_p(const TSNode *node);
uint32_t occam_node_child_count_p(const TSNode *node);
void occam_node_child_p(const TSNode *node, uint32_t index, TSNode *out);
bool occam_node_is_named_p(const TSNode *node);
bool occam_node_is_null_p(const TSNode *node);

#ifdef __cplusplus
}
#endif

#endif // OCCAM_TS_H

#include <stdint.h>
#include <stddef.h>
#include <string.h>

/* NOTE: Keep the by-value calls in C. These layouts mirror the DuckDB C ABI. */
typedef struct {
    uint64_t column_count, row_count, rows_changed;
    void *columns;
    char *error_message;
    void *internal_data;
} occam_duckdb_result;

typedef union {
    struct { uint32_t length; char prefix[4]; char *ptr; } pointer;
    struct { uint32_t length; char inlined[12]; } inline_value;
} occam_duckdb_string;

int occam_duckdb_result_kind(int (*kind)(occam_duckdb_result), const occam_duckdb_result *result) {
    return kind(*result);
}

struct _duckdb_data_chunk;
struct _duckdb_data_chunk *occam_duckdb_fetch_chunk(
    struct _duckdb_data_chunk *(*fetch)(occam_duckdb_result),
    const occam_duckdb_result *result
) {
    return fetch(*result);
}

const char *occam_duckdb_vector_bytes(
    const char *(*get)(occam_duckdb_string *),
    occam_duckdb_string *values, uint64_t row, uint64_t *size
) {
    occam_duckdb_string *value = values + row;
    uint32_t length;
    memcpy(&length, value, sizeof(length));
    *size = length;
    return get(value);
}

size_t occam_duckdb_result_size(void) {
    return sizeof(occam_duckdb_result);
}

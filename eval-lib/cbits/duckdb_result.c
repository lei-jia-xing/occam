#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
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

typedef struct _duckdb_data_chunk *duckdb_chunk;
typedef struct _duckdb_vector *duckdb_vector;

typedef struct {
    const void *data;
    const uint64_t *validity;
} occam_duckdb_values;

/* Scalars and lengths are unsigned 64-bit words in little-endian order. */
static int read_word(const unsigned char **cursor, const unsigned char *end, uint64_t *out) {
    const unsigned char *p = *cursor;
    if (end - p < 8)
        return 0;
    uint64_t word = 0;
    for (unsigned int i = 0; i < 8; ++i)
        word |= (uint64_t)p[i] << (8 * i);
    *out = word;
    *cursor = p + 8;
    return 1;
}

static int match_word(const unsigned char **cursor, const unsigned char *end, uint64_t value) {
    uint64_t expected;
    return read_word(cursor, end, &expected) && expected == value;
}

static uint64_t double_bits(double value) {
    uint64_t bits;
    memcpy(&bits, &value, sizeof(bits));
    return bits;
}

static int match_cell(
    const unsigned char **cursor, const unsigned char *end, int type,
    occam_duckdb_values values, uint64_t row,
    const char *(*string_data)(occam_duckdb_string *)
) {
    if (*cursor == end)
        return 0;
    const int actual_null = type == 36 ||
        (values.validity && !(values.validity[row / 64] & (UINT64_C(1) << (row % 64))));
    if (*(*cursor)++ != (unsigned char)actual_null)
        return 0;
    if (actual_null)
        return 1;

    const void *data = values.data;
    switch (type) {
    case 1: return match_word(cursor, end, ((const uint8_t *)data)[row] != 0);
    case 2: return match_word(cursor, end, (uint64_t)((const int8_t *)data)[row]);
    case 3: return match_word(cursor, end, (uint64_t)((const int16_t *)data)[row]);
    case 4: return match_word(cursor, end, (uint64_t)((const int32_t *)data)[row]);
    case 5: return match_word(cursor, end, (uint64_t)((const int64_t *)data)[row]);
    case 6: return match_word(cursor, end, ((const uint8_t *)data)[row]);
    case 7: return match_word(cursor, end, ((const uint16_t *)data)[row]);
    case 8: return match_word(cursor, end, ((const uint32_t *)data)[row]);
    case 9: return match_word(cursor, end, ((const uint64_t *)data)[row]);
    case 10: return match_word(cursor, end, double_bits(((const float *)data)[row]));
    case 11: return match_word(cursor, end, double_bits(((const double *)data)[row]));
    case 16:
    case 32: {
        uint64_t words[2];
        memcpy(words, (const unsigned char *)data + row * sizeof(words), sizeof(words));
        return match_word(cursor, end, words[0]) && match_word(cursor, end, words[1]);
    }
    case 17:
    case 18: {
        uint64_t expected, length;
        const char *bytes = occam_duckdb_vector_bytes(string_data, (void *)data, row, &length);
        if (!read_word(cursor, end, &expected) || expected != length ||
            length > (uint64_t)(end - *cursor))
            return 0;
        if (length && memcmp(*cursor, bytes, (size_t)length) != 0)
            return 0;
        *cursor += length;
        return 1;
    }
    default:
        return 0;
    }
}

/* The caller checks the column names and types before consuming any chunks.
 * Returns -1 on allocation failure, 0 on mismatch, and 1 on an exact match.
 */
int occam_duckdb_match_rows(
    duckdb_chunk (*fetch)(occam_duckdb_result),
    void (*destroy)(duckdb_chunk *),
    uint64_t (*chunk_size)(duckdb_chunk),
    duckdb_vector (*chunk_vector)(duckdb_chunk, uint64_t),
    void *(*vector_data)(duckdb_vector),
    uint64_t *(*vector_validity)(duckdb_vector),
    const char *(*string_data)(occam_duckdb_string *),
    const occam_duckdb_result *result,
    const int *types, size_t columns,
    const unsigned char *expected, size_t expected_length
) {
    const unsigned char *end = expected + expected_length;
    uint64_t remaining;
    if (!read_word(&expected, end, &remaining))
        return 0;
    occam_duckdb_values *vectors = calloc(columns ? columns : 1, sizeof(*vectors));
    if (!vectors)
        return -1;
    int match = 1;
    duckdb_chunk chunk;
    while (match && (chunk = fetch(*result)) != NULL) {
        const uint64_t rows = chunk_size(chunk);
        if (rows > remaining) {
            match = 0;
        } else {
            remaining -= rows;
            for (size_t col = 0; col < columns; ++col) {
                duckdb_vector vector = chunk_vector(chunk, col);
                vectors[col].data = vector_data(vector);
                vectors[col].validity = vector_validity(vector);
            }
            for (uint64_t row = 0; match && row < rows; ++row)
                for (size_t col = 0; col < columns; ++col)
                    if (!match_cell(&expected, end, types[col], vectors[col], row, string_data)) {
                        match = 0;
                        break;
                    }
        }
        destroy(&chunk);
    }
    free(vectors);
    return match && remaining == 0 && expected == end;
}

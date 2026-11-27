#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* NOTE: Keep the by-value libpg_query calls in C without requiring pg_query.h.
 * These layouts mirror the libpg_query 18 C ABI exactly.
 */
typedef struct {
    char *message;
    char *funcname;
    char *filename;
    int lineno;
    int cursorpos;
    char *context;
} occam_pg_query_error;

typedef struct {
    char *parse_tree;
    char *stderr_buffer;
    occam_pg_query_error *error;
} occam_pg_query_parse_result;

typedef struct {
    int stmt_location;
    int stmt_len;
} occam_pg_query_split_stmt;

typedef struct {
    occam_pg_query_split_stmt **stmts;
    int n_stmts;
    char *stderr_buffer;
    occam_pg_query_error *error;
} occam_pg_query_split_result;

static void copy_error_message(
    const occam_pg_query_error *err,
    int *out_cursorpos,
    char *err_buf,
    size_t err_buf_len
) {
    if (out_cursorpos != NULL) {
        *out_cursorpos = err != NULL ? err->cursorpos : 0;
    }
    if (err_buf != NULL && err_buf_len > 0) {
        const char *msg =
            (err != NULL && err->message != NULL) ? err->message : "PostgreSQL syntax error";
        size_t len = strlen(msg);
        if (len >= err_buf_len) {
            len = err_buf_len - 1;
        }
        memcpy(err_buf, msg, len);
        err_buf[len] = '\0';
    }
}

int occam_pg_query_split_check(
    occam_pg_query_split_result (*split_fn)(const char *),
    void (*free_fn)(occam_pg_query_split_result),
    const char *sql,
    int *out_n_stmts,
    int *out_cursorpos,
    char *err_buf,
    size_t err_buf_len
) {
    occam_pg_query_split_result res = split_fn(sql);
    int status = 0;
    if (res.error != NULL) {
        status = 1;
        if (out_n_stmts != NULL) {
            *out_n_stmts = 0;
        }
        copy_error_message(res.error, out_cursorpos, err_buf, err_buf_len);
    } else {
        if (out_n_stmts != NULL) {
            *out_n_stmts = res.n_stmts;
        }
        if (out_cursorpos != NULL) {
            *out_cursorpos = 0;
        }
        if (err_buf != NULL && err_buf_len > 0) {
            err_buf[0] = '\0';
        }
    }
    free_fn(res);
    return status;
}

int occam_pg_query_parse_check(
    occam_pg_query_parse_result (*parse_fn)(const char *),
    void (*free_fn)(occam_pg_query_parse_result),
    const char *sql,
    int *out_cursorpos,
    char *err_buf,
    size_t err_buf_len
) {
    occam_pg_query_parse_result res = parse_fn(sql);
    int status = 0;
    if (res.error != NULL) {
        status = 1;
        copy_error_message(res.error, out_cursorpos, err_buf, err_buf_len);
    } else {
        if (out_cursorpos != NULL) {
            *out_cursorpos = 0;
        }
        if (err_buf != NULL && err_buf_len > 0) {
            err_buf[0] = '\0';
        }
    }
    free_fn(res);
    return status;
}

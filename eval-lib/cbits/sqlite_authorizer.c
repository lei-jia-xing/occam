#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#if defined(_WIN32)
#include <windows.h>
#else
#include <time.h>
#endif

/*
 * Conservative ASCII case-insensitive scan for SQLite wall-clock keywords
 * (CURRENT_TIME, CURRENT_DATE, CURRENT_TIMESTAMP) that may bypass the
 * SQLITE_FUNCTION authorizer callback.
 */
int occam_sqlite_has_nondet_keyword(const uint8_t *buf, size_t len) {
    static const char k_time[] = "current_time";
    static const char k_date[] = "current_date";
    const size_t klen = 12;
    if (len < klen) {
        return 0;
    }
    for (size_t i = 0; i + klen <= len; i++) {
        uint8_t b0 = buf[i];
        if (b0 != 'c' && b0 != 'C') {
            continue;
        }
        int match_time = 1;
        int match_date = 1;
        for (size_t j = 1; j < klen && (match_time || match_date); j++) {
            uint8_t c = buf[i + j];
            if (c >= 'A' && c <= 'Z') {
                c = (uint8_t)(c + ('a' - 'A'));
            }
            if (match_time && c != (uint8_t)k_time[j]) {
                match_time = 0;
            }
            if (match_date && c != (uint8_t)k_date[j]) {
                match_date = 0;
            }
        }
        if (match_time || match_date) {
            return 1;
        }
    }
    return 0;
}

/* Returns 0 if valid UTF-8 without NUL, 1 if NUL byte present, 2 if invalid UTF-8. */
int occam_sqlite_check_sql(const uint8_t *buf, size_t len) {
    size_t i = 0;
    while (i < len) {
        uint8_t b0 = buf[i];
        if (b0 == 0) {
            return 1;
        }
        if (b0 < 0x80) {
            i++;
            continue;
        }
        if (b0 >= 0xC2 && b0 <= 0xDF) {
            if (i + 1 >= len) return 2;
            uint8_t b1 = buf[i + 1];
            if ((b1 & 0xC0) != 0x80) return 2;
            i += 2;
        } else if (b0 >= 0xE0 && b0 <= 0xEF) {
            if (i + 2 >= len) return 2;
            uint8_t b1 = buf[i + 1];
            uint8_t b2 = buf[i + 2];
            if ((b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80) return 2;
            if (b0 == 0xE0 && b1 < 0xA0) return 2;
            if (b0 == 0xED && b1 > 0x9F) return 2;
            i += 3;
        } else if (b0 >= 0xF0 && b0 <= 0xF4) {
            if (i + 3 >= len) return 2;
            uint8_t b1 = buf[i + 1];
            uint8_t b2 = buf[i + 2];
            uint8_t b3 = buf[i + 3];
            if ((b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80 || (b3 & 0xC0) != 0x80) return 2;
            if (b0 == 0xF0 && b1 < 0x90) return 2;
            if (b0 == 0xF4 && b1 > 0x8F) return 2;
            i += 4;
        } else {
            return 2;
        }
    }
    return 0;
}

#define OCCAM_DIRTY_MAIN 1
#define OCCAM_DIRTY_PRAGMA 2
#define OCCAM_DIRTY_TEMP 4
#define OCCAM_DIRTY_DML 8
#define OCCAM_DIRTY_CONN_COUNTER 16
#define OCCAM_DIRTY_NONDET 32

static int ascii_eq_ci(const char *a, const char *b) {
    if (!a || !b) {
        return 0;
    }
    while (*a && *b) {
        unsigned char ca = (unsigned char)*a;
        unsigned char cb = (unsigned char)*b;
        if (ca >= 'A' && ca <= 'Z') {
            ca = (unsigned char)(ca + ('a' - 'A'));
        }
        if (cb >= 'A' && cb <= 'Z') {
            cb = (unsigned char)(cb + ('a' - 'A'));
        }
        if (ca != cb) {
            return 0;
        }
        a++;
        b++;
    }
    return *a == '\0' && *b == '\0';
}

static int cstr_is_temp(const char *p) {
    if (!p) {
        return 0;
    }
    return (p[0] == 't' || p[0] == 'T')
        && (p[1] == 'e' || p[1] == 'E')
        && (p[2] == 'm' || p[2] == 'M')
        && (p[3] == 'p' || p[3] == 'P')
        && p[4] == '\0';
}

int occam_sqlite_authorizer(
    void *user_data,
    int op,
    const char *arg1,
    const char *arg2,
    const char *arg3,
    const char *arg4
) {
    (void)arg4;
    if (!user_data) {
        return 0;
    }
    int *dirty = (int *)user_data;
    switch (op) {
        case 19: /* SQLITE_PRAGMA */
            if (arg1 != NULL
                && (ascii_eq_ci(arg1, "foreign_keys")
                    || ascii_eq_ci(arg1, "defer_foreign_keys")
                    || ascii_eq_ci(arg1, "recursive_triggers")
                    || ascii_eq_ci(arg1, "user_version"))) {
                if (cstr_is_temp(arg3)) {
                    *dirty |= OCCAM_DIRTY_TEMP;
                }
                if (arg2 != NULL) {
                    *dirty |= OCCAM_DIRTY_PRAGMA;
                }
            } else {
                *dirty |= (OCCAM_DIRTY_TEMP | OCCAM_DIRTY_NONDET);
            }
            return 0;
        case 24: /* SQLITE_ATTACH */
        case 25: /* SQLITE_DETACH */
        case 29: /* SQLITE_CREATE_VTABLE */
        case 30: /* SQLITE_DROP_VTABLE */
            *dirty |= (OCCAM_DIRTY_MAIN | OCCAM_DIRTY_TEMP | OCCAM_DIRTY_NONDET);
            return 0;
        case 3:  /* SQLITE_CREATE_TEMP_INDEX */
        case 4:  /* SQLITE_CREATE_TEMP_TABLE */
        case 5:  /* SQLITE_CREATE_TEMP_TRIGGER */
        case 6:  /* SQLITE_CREATE_TEMP_VIEW */
        case 12: /* SQLITE_DROP_TEMP_INDEX */
        case 13: /* SQLITE_DROP_TEMP_TABLE */
        case 14: /* SQLITE_DROP_TEMP_TRIGGER */
        case 15: /* SQLITE_DROP_TEMP_VIEW */
            *dirty |= (OCCAM_DIRTY_MAIN | OCCAM_DIRTY_TEMP);
            return 0;
        case 20: /* SQLITE_READ */
        case 21: /* SQLITE_SELECT */
        case 22: /* SQLITE_TRANSACTION */
        case 31: /* SQLITE_FUNCTION */
        case 32: /* SQLITE_SAVEPOINT */
        case 33: /* SQLITE_RECURSIVE */
            if (cstr_is_temp(arg3)) {
                *dirty |= OCCAM_DIRTY_TEMP;
            }
            if (op == 31 && arg2 != NULL) {
                if (ascii_eq_ci(arg2, "last_insert_rowid")
                    || ascii_eq_ci(arg2, "changes")
                    || ascii_eq_ci(arg2, "total_changes")) {
                    *dirty |= OCCAM_DIRTY_CONN_COUNTER;
                }
                if (ascii_eq_ci(arg2, "random")
                    || ascii_eq_ci(arg2, "randomblob")
                    || ascii_eq_ci(arg2, "date")
                    || ascii_eq_ci(arg2, "time")
                    || ascii_eq_ci(arg2, "datetime")
                    || ascii_eq_ci(arg2, "julianday")
                    || ascii_eq_ci(arg2, "strftime")
                    || ascii_eq_ci(arg2, "unixepoch")
                    || ascii_eq_ci(arg2, "timediff")) {
                    *dirty |= OCCAM_DIRTY_NONDET;
                }
            }
            return 0;
        case 9:  /* SQLITE_DELETE */
        case 18: /* SQLITE_INSERT */
        case 23: /* SQLITE_UPDATE */
            *dirty |= OCCAM_DIRTY_MAIN | OCCAM_DIRTY_DML | (cstr_is_temp(arg3) ? OCCAM_DIRTY_TEMP : 0);
            return 0;
        default:
            *dirty |= OCCAM_DIRTY_MAIN | (cstr_is_temp(arg3) ? OCCAM_DIRTY_TEMP : 0);
            return 0;
    }
}

typedef struct sqlite3 sqlite3;
typedef struct sqlite3_stmt sqlite3_stmt;
typedef struct sqlite3_backup sqlite3_backup;

extern int sqlite3_open(const char *filename, sqlite3 **ppDb);
extern int sqlite3_close(sqlite3 *);
extern void sqlite3_progress_handler(sqlite3 *, int, int (*)(void *), void *);
extern int sqlite3_get_autocommit(sqlite3 *);
extern int sqlite3_limit(sqlite3 *, int id, int newVal);
extern int sqlite3_exec(sqlite3 *, const char *sql, int (*callback)(void *, int, char **, char **), void *, char **errmsg);
extern sqlite3_backup *sqlite3_backup_init(sqlite3 *pDest, const char *zDestName, sqlite3 *pSource, const char *zSourceName);
extern int sqlite3_backup_step(sqlite3_backup *p, int nPage);
extern int sqlite3_backup_finish(sqlite3_backup *p);
extern int sqlite3_prepare_v2(sqlite3 *db, const char *zSql, int nByte, sqlite3_stmt **ppStmt, const char **pzTail);
extern int sqlite3_column_count(sqlite3_stmt *pStmt);
extern const char *sqlite3_column_name(sqlite3_stmt *pStmt, int N);
extern int sqlite3_column_type(sqlite3_stmt *pStmt, int iCol);
extern int64_t sqlite3_column_int64(sqlite3_stmt *pStmt, int iCol);
extern double sqlite3_column_double(sqlite3_stmt *pStmt, int iCol);
extern const unsigned char *sqlite3_column_text(sqlite3_stmt *pStmt, int iCol);
extern const void *sqlite3_column_blob(sqlite3_stmt *pStmt, int iCol);
extern int sqlite3_column_bytes(sqlite3_stmt *pStmt, int iCol);
extern int sqlite3_step(sqlite3_stmt *pStmt);
extern int sqlite3_finalize(sqlite3_stmt *pStmt);

/* Returns 1 if connection reset cleanly and is poolable, 0 if connection must be destroyed. */
int occam_sqlite_reset(sqlite3 *db, sqlite3 *tmpl, int *dirty_ptr) {
    int dirty = *dirty_ptr;
    if ((dirty & OCCAM_DIRTY_TEMP) != 0) {
        return 0;
    }
    if (sqlite3_get_autocommit(db) == 0) {
        if (sqlite3_exec(db, "ROLLBACK;", NULL, NULL, NULL) != 0) {
            return 0;
        }
    }
    if ((dirty & OCCAM_DIRTY_PRAGMA) != 0) {
        if (sqlite3_exec(
                db,
                "PRAGMA foreign_keys = OFF; PRAGMA defer_foreign_keys = OFF; PRAGMA recursive_triggers = OFF; PRAGMA user_version = 0;",
                NULL,
                NULL,
                NULL
            ) != 0) {
            return 0;
        }
    }
    if ((dirty & OCCAM_DIRTY_MAIN) != 0) {
        sqlite3_backup *bk = sqlite3_backup_init(db, "main", tmpl, "main");
        if (bk == NULL) {
            return 0;
        }
        int step_rc = sqlite3_backup_step(bk, -1);
        int finish_rc = sqlite3_backup_finish(bk);
        if (step_rc != 101 || finish_rc != 0) {
            return 0;
        }
    }
    *dirty_ptr = 0;
    return 1;
}

/*
 * Advances through [start, end), executing zero-column statements in C.
 * Returns:
 *   0  = finished all statements cleanly (*out_stmt = NULL)
 *   1  = stopped at a statement with column_count > 0 (*out_stmt non-NULL,
 *        *out_tail = unconsumed tail, *out_ordinal = this statement's ordinal)
 *  -1  = prepare error (*out_stmt = NULL)
 *  -2  = step error (*out_stmt non-NULL; caller reads errmsg/errcode then finalizes)
 */
int occam_sqlite_next_result(
    sqlite3 *db,
    const char *base,
    const char *start,
    const char *end,
    int ordinal,
    int *end_offsets,
    int max_offsets,
    const char **out_tail,
    sqlite3_stmt **out_stmt,
    int *out_ordinal
) {
    int sql_limit = sqlite3_limit(db, 1 /* SQLITE_LIMIT_SQL_LENGTH */, -1);
    while (start < end) {
        sqlite3_stmt *stmt = NULL;
        const char *tail = start;
        ptrdiff_t rem = end - start;
        /* Haskell provides a NUL at end. Include it to avoid SQLite copying
         * the entire remaining script for each prepare. Keep SQLite's
         * length check for buffers exceeding its configured SQL limit. */
        int nbyte = rem <= sql_limit && rem < INT_MAX
            ? (int)rem + 1
            : (rem > INT_MAX ? INT_MAX : (int)rem);
        int rc = sqlite3_prepare_v2(db, start, nbyte, &stmt, &tail);
        if (rc != 0) {
            *out_stmt = NULL;
            *out_tail = tail;
            *out_ordinal = ordinal;
            return -1;
        }
        if (stmt == NULL) {
            if (tail <= start) {
                break;
            }
            start = tail;
            continue;
        }
        if (end_offsets != NULL && ordinal >= 0 && ordinal < max_offsets) {
            end_offsets[ordinal] = (int)(tail - base);
        }
        if (sqlite3_column_count(stmt) == 0) {
            while ((rc = sqlite3_step(stmt)) == 100) {
            }
            if (rc != 101) {
                *out_stmt = stmt;
                *out_tail = tail;
                *out_ordinal = ordinal;
                return -2;
            }
            sqlite3_finalize(stmt);
            ordinal++;
            if (tail <= start) {
                break;
            }
            start = tail;
        } else {
            *out_stmt = stmt;
            *out_tail = tail;
            *out_ordinal = ordinal;
            return 1;
        }
    }
    *out_stmt = NULL;
    *out_tail = end;
    *out_ordinal = ordinal;
    return 0;
}

static int64_t occam_monotonic_ns(void) {
#if defined(_WIN32)
    LARGE_INTEGER freq, counter;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&counter);
    return (int64_t)((double)counter.QuadPart * 1000000000.0 / (double)freq.QuadPart);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000000000LL + (int64_t)ts.tv_nsec;
#endif
}

typedef struct {
    int64_t deadline_ns;
    int timed_out;
} occam_sqlite_deadline;

static int occam_sqlite_progress_cb(void *ctx) {
    occam_sqlite_deadline *dl = (occam_sqlite_deadline *)ctx;
    if (occam_monotonic_ns() >= dl->deadline_ns) {
        dl->timed_out = 1;
        return 1;
    }
    return 0;
}

#define OCCAM_MAX_CKPTS 16

typedef struct {
    sqlite3 *db;
    size_t prefix_len;
    int stmt_count;
    int first_result_idx;
    int last_result_idx;
    int has_result;
    int last_matched;
    int had_dml;
    int *end_offsets;
    uint8_t *result_flags;
} occam_sqlite_ckpt;

typedef struct {
    char *sql_buf;
    size_t sql_cap;
    uint8_t *base_sql;
    size_t base_len;
    occam_sqlite_ckpt ckpts[OCCAM_MAX_CKPTS];
    int num_ckpts;
} occam_sqlite_cache;

occam_sqlite_cache *occam_sqlite_cache_create(void) {
    return (occam_sqlite_cache *)calloc(1, sizeof(occam_sqlite_cache));
}

void occam_sqlite_cache_clear_ckpts(occam_sqlite_cache *cache) {
    if (!cache) {
        return;
    }
    for (int i = 0; i < cache->num_ckpts; i++) {
        if (cache->ckpts[i].db) {
            sqlite3_close(cache->ckpts[i].db);
            cache->ckpts[i].db = NULL;
        }
        free(cache->ckpts[i].end_offsets);
        cache->ckpts[i].end_offsets = NULL;
        free(cache->ckpts[i].result_flags);
        cache->ckpts[i].result_flags = NULL;
    }
    cache->num_ckpts = 0;
    free(cache->base_sql);
    cache->base_sql = NULL;
    cache->base_len = 0;
}

void occam_sqlite_cache_destroy(occam_sqlite_cache *cache) {
    if (!cache) {
        return;
    }
    occam_sqlite_cache_clear_ckpts(cache);
    free(cache->sql_buf);
    free(cache);
}

static int cache_ensure_sql_buf(occam_sqlite_cache *cache, const uint8_t *sql, size_t sql_len) {
    if (cache->sql_cap < sql_len + 1) {
        size_t new_cap = (sql_len + 1) < 1024 ? 1024 : (sql_len + 1) * 2;
        char *nb = (char *)realloc(cache->sql_buf, new_cap);
        if (!nb) {
            return 0;
        }
        cache->sql_buf = nb;
        cache->sql_cap = new_cap;
    }
    if (sql_len > 0) {
        memcpy(cache->sql_buf, sql, sql_len);
    }
    cache->sql_buf[sql_len] = '\0';
    return 1;
}

static int read_u32_le(const uint8_t *buf, size_t len, size_t *pos, uint32_t *out) {
    if (*pos + 4 > len) {
        return 0;
    }
    memcpy(out, buf + *pos, 4);
    *pos += 4;
    return 1;
}

static int read_u64_le(const uint8_t *buf, size_t len, size_t *pos, uint64_t *out) {
    if (*pos + 8 > len) {
        return 0;
    }
    memcpy(out, buf + *pos, 8);
    *pos += 8;
    return 1;
}

/*
 * Step a result-bearing statement to completion and compare columns + rows
 * against a compact binary expected buffer.
 *
 * Returns:
 *   0  = stepped cleanly to SQLITE_DONE (*out_matched = 1 or 0)
 *  -1  = step error (rc in *out_rc; caller inspects timeout vs SQL error)
 *  -2  = infrastructure error (NULL column name or malformed expected buffer)
 */
static int match_stmt_against_expected(
    sqlite3_stmt *stmt,
    const uint8_t *exp_buf,
    size_t exp_len,
    int *out_matched,
    int *out_rc
) {
    *out_matched = 0;
    *out_rc = 0;

    int col_count = sqlite3_column_count(stmt);
    size_t pos = 0;
    uint32_t exp_cols = 0;
    uint32_t exp_rows = 0;
    if (!read_u32_le(exp_buf, exp_len, &pos, &exp_cols) ||
        !read_u32_le(exp_buf, exp_len, &pos, &exp_rows)) {
        return -2;
    }

    int matched = ((uint32_t)col_count == exp_cols);

    /* Validate column names (also check non-NULL column names unconditionally). */
    for (int c = 0; c < col_count; c++) {
        const char *cname = sqlite3_column_name(stmt, c);
        if (cname == NULL) {
            return -2;
        }
        if (matched) {
            uint32_t name_len = 0;
            if (!read_u32_le(exp_buf, exp_len, &pos, &name_len) || pos + name_len > exp_len) {
                return -2;
            }
            size_t clen = strlen(cname);
            if (clen != (size_t)name_len || (name_len > 0 && memcmp(cname, exp_buf + pos, name_len) != 0)) {
                matched = 0;
            }
            pos += name_len;
        }
    }

    uint32_t row_idx = 0;
    int rc;
    while ((rc = sqlite3_step(stmt)) == 100 /* SQLITE_ROW */) {
        if (!matched) {
            continue;
        }
        if (row_idx >= exp_rows) {
            matched = 0;
            continue;
        }
        for (int c = 0; c < col_count; c++) {
            if (pos >= exp_len) {
                return -2;
            }
            uint8_t exp_tag = exp_buf[pos++];
            int actual_tag = sqlite3_column_type(stmt, c);
            if (actual_tag != (int)exp_tag) {
                matched = 0;
                break;
            }
            if (exp_tag == 1 /* SQLITE_INTEGER */) {
                uint64_t exp_u = 0;
                if (!read_u64_le(exp_buf, exp_len, &pos, &exp_u)) {
                    return -2;
                }
                int64_t exp_i = 0;
                memcpy(&exp_i, &exp_u, 8);
                if (sqlite3_column_int64(stmt, c) != exp_i) {
                    matched = 0;
                    break;
                }
            } else if (exp_tag == 2 /* SQLITE_FLOAT */) {
                uint64_t exp_bits = 0;
                if (!read_u64_le(exp_buf, exp_len, &pos, &exp_bits)) {
                    return -2;
                }
                double d = sqlite3_column_double(stmt, c);
                uint64_t actual_bits = 0;
                memcpy(&actual_bits, &d, 8);
                if (actual_bits != exp_bits) {
                    matched = 0;
                    break;
                }
            } else if (exp_tag == 3 /* SQLITE_TEXT */) {
                uint32_t blen = 0;
                if (!read_u32_le(exp_buf, exp_len, &pos, &blen) || pos + blen > exp_len) {
                    return -2;
                }
                const unsigned char *txt = sqlite3_column_text(stmt, c);
                int alen = sqlite3_column_bytes(stmt, c);
                if (alen != (int)blen || (blen > 0 && (txt == NULL || memcmp(txt, exp_buf + pos, blen) != 0))) {
                    matched = 0;
                    break;
                }
                pos += blen;
            } else if (exp_tag == 4 /* SQLITE_BLOB */) {
                uint32_t blen = 0;
                if (!read_u32_le(exp_buf, exp_len, &pos, &blen) || pos + blen > exp_len) {
                    return -2;
                }
                const void *blob = sqlite3_column_blob(stmt, c);
                int alen = sqlite3_column_bytes(stmt, c);
                if (alen != (int)blen || (blen > 0 && (blob == NULL || memcmp(blob, exp_buf + pos, blen) != 0))) {
                    matched = 0;
                    break;
                }
                pos += blen;
            } else if (exp_tag == 5 /* SQLITE_NULL */) {
                /* Type match is sufficient. */
            } else {
                return -2;
            }
        }
        row_idx++;
    }

    if (rc != 101 /* SQLITE_DONE */) {
        *out_rc = rc;
        return -1;
    }

    *out_matched = (matched && row_idx == exp_rows) ? 1 : 0;
    return 0;
}

static int backup_db_main(sqlite3 *dest, sqlite3 *src) {
    sqlite3_backup *bk = sqlite3_backup_init(dest, "main", src, "main");
    if (!bk) {
        return 0;
    }
    int step_rc = sqlite3_backup_step(bk, -1);
    int finish_rc = sqlite3_backup_finish(bk);
    return (step_rc == 101 && finish_rc == 0) ? 1 : 0;
}

/*
 * Execute a full candidate SQL script in C against `db`, matching every
 * result-bearing statement against `expected_buf`, optionally resuming from
 * a matching deterministic prefix checkpoint in `cache`.
 *
 * Returns:
 *   0 = finished cleanly AND final result matched expected
 *   1 = finished cleanly AND final result mismatched (or no result set)
 *   2 = SQL prepare/step error (*out_fail_end_offset set if semicolon-terminated)
 *   3 = timeout (progress handler deadline) or interrupt
 *   4 = infrastructure error (OOM / NULL column name / backup failure)
 */
int occam_sqlite_run_and_match(
    sqlite3 *db,
    int *dirty_ptr,
    occam_sqlite_cache *cache,
    const uint8_t *sql,
    size_t sql_len,
    const uint8_t *expected_buf,
    size_t expected_len,
    int64_t timeout_micros,
    int allow_ckpt_use,
    int *out_stmt_count,
    int *out_first_result_idx,
    int *out_last_result_idx,
    int *out_end_offsets,
    uint8_t *out_result_flags,
    int max_offsets,
    int *out_used_ckpt_dml,
    int *out_fail_end_offset
) {
    *out_stmt_count = 0;
    *out_first_result_idx = -1;
    *out_last_result_idx = -1;
    *out_used_ckpt_dml = 0;
    *out_fail_end_offset = 0;

    if (!cache || !cache_ensure_sql_buf(cache, sql, sql_len)) {
        return 4;
    }

    const char *base = cache->sql_buf;
    const char *end = base + sql_len;
    const char *start = base;
    int ordinal = 0;
    int first_result = -1;
    int last_result = -1;
    int has_result = 0;
    int last_matched = 0;
    const occam_sqlite_ckpt *matched_ckpt = NULL;

    occam_sqlite_deadline dl;
    dl.deadline_ns = occam_monotonic_ns() + (timeout_micros > 0 ? timeout_micros * 1000LL : 0);
    dl.timed_out = 0;
    if (timeout_micros <= 0) {
        return 3;
    }
    sqlite3_progress_handler(db, 1000, occam_sqlite_progress_cb, &dl);

    /* Find longest matching deterministic semicolon-terminated prefix checkpoint. */
    if (allow_ckpt_use && cache->num_ckpts > 0 && cache->base_sql != NULL) {
        for (int i = cache->num_ckpts - 1; i >= 0; i--) {
            size_t plen = cache->ckpts[i].prefix_len;
            if (plen > 0 && plen <= sql_len && memcmp(sql, cache->base_sql, plen) == 0) {
                if (backup_db_main(db, cache->ckpts[i].db)) {
                    matched_ckpt = &cache->ckpts[i];
                    *dirty_ptr |= OCCAM_DIRTY_MAIN;
                    start = base + plen;
                    ordinal = matched_ckpt->stmt_count;
                    first_result = matched_ckpt->first_result_idx;
                    last_result = matched_ckpt->last_result_idx;
                    has_result = matched_ckpt->has_result;
                    last_matched = matched_ckpt->last_matched;
                    *out_used_ckpt_dml = matched_ckpt->had_dml;
                }
                break;
            }
        }
    }

    int sql_limit = sqlite3_limit(db, 1 /* SQLITE_LIMIT_SQL_LENGTH */, -1);
    int status = 1;

    while (start < end) {
        sqlite3_stmt *stmt = NULL;
        const char *tail = start;
        ptrdiff_t rem = end - start;
        int nbyte = rem <= sql_limit && rem < INT_MAX
            ? (int)rem + 1
            : (rem > INT_MAX ? INT_MAX : (int)rem);
        int rc = sqlite3_prepare_v2(db, start, nbyte, &stmt, &tail);
        if (rc != 0) {
            if (rc == 9 /* SQLITE_INTERRUPT */) {
                status = 3;
            } else {
                if (tail > start && tail <= end && tail[-1] == ';') {
                    *out_fail_end_offset = (int)(tail - base);
                }
                status = 2;
            }
            goto done;
        }
        if (stmt == NULL) {
            if (tail <= start) {
                break;
            }
            start = tail;
            continue;
        }
        if (out_end_offsets != NULL && ordinal >= 0 && ordinal < max_offsets) {
            out_end_offsets[ordinal] = (int)(tail - base);
        }
        if (sqlite3_column_count(stmt) == 0) {
            while ((rc = sqlite3_step(stmt)) == 100 /* SQLITE_ROW */) {
            }
            sqlite3_finalize(stmt);
            if (rc != 101 /* SQLITE_DONE */) {
                if (rc == 9 /* SQLITE_INTERRUPT */) {
                    status = 3;
                } else {
                    if (tail > base && tail <= end && tail[-1] == ';') {
                        *out_fail_end_offset = (int)(tail - base);
                    }
                    status = 2;
                }
                goto done;
            }
            if (out_result_flags != NULL && ordinal >= 0 && ordinal < max_offsets) {
                out_result_flags[ordinal] = 0;
            }
        } else {
            int stmt_matched = 0;
            int step_rc = 0;
            int mrc = match_stmt_against_expected(stmt, expected_buf, expected_len, &stmt_matched, &step_rc);
            sqlite3_finalize(stmt);
            if (mrc == -2) {
                status = 4;
                goto done;
            }
            if (mrc == -1) {
                if (step_rc == 9 /* SQLITE_INTERRUPT */) {
                    status = 3;
                } else {
                    if (tail > base && tail <= end && tail[-1] == ';') {
                        *out_fail_end_offset = (int)(tail - base);
                    }
                    status = 2;
                }
                goto done;
            }
            has_result = 1;
            if (first_result < 0) {
                first_result = ordinal;
            }
            last_result = ordinal;
            last_matched = stmt_matched;
            if (out_result_flags != NULL && ordinal >= 0 && ordinal < max_offsets) {
                out_result_flags[ordinal] = (uint8_t)(1 | (stmt_matched ? 2 : 0));
            }
        }
        ordinal++;
        if (tail <= start) {
            break;
        }
        start = tail;
    }

    if (has_result && last_matched) {
        if (matched_ckpt != NULL) {
            int copy_n = matched_ckpt->stmt_count;
            if (copy_n > max_offsets) {
                copy_n = max_offsets;
            }
            if (out_end_offsets != NULL && matched_ckpt->end_offsets != NULL && copy_n > 0) {
                memcpy(out_end_offsets, matched_ckpt->end_offsets, (size_t)copy_n * sizeof(int));
            }
            if (out_result_flags != NULL && matched_ckpt->result_flags != NULL && copy_n > 0) {
                memcpy(out_result_flags, matched_ckpt->result_flags, (size_t)copy_n);
            }
        }
        status = 0;
    } else {
        status = 1;
    }

done:
    sqlite3_progress_handler(db, 0, NULL, NULL);
    *out_stmt_count = ordinal;
    *out_first_result_idx = first_result;
    *out_last_result_idx = last_result;
    return status;
}

/*
 * Build up to OCCAM_MAX_CKPTS evenly spaced deterministic prefix checkpoints
 * for a clean accepted script `sql` (must already have been verified clean &
 * deterministic). Caller must pass a freshly reset `db`; this function resets
 * `db` back to `tmpl` before returning.
 */
int occam_sqlite_build_ckpts(
    sqlite3 *db,
    sqlite3 *tmpl,
    int *dirty_ptr,
    occam_sqlite_cache *cache,
    const uint8_t *sql,
    size_t sql_len,
    const uint8_t *expected_buf,
    size_t expected_len,
    int total_stmts
) {
    if (!cache || total_stmts < 8 || sql_len == 0) {
        return 1;
    }
    /* Skip rebuild if checkpoints already match this exact base script. */
    if (cache->base_sql != NULL && cache->base_len == sql_len &&
        memcmp(cache->base_sql, sql, sql_len) == 0 && cache->num_ckpts > 0) {
        return 1;
    }

    /* Retain valid prefix checkpoints if new candidate shares prefix byte-for-byte. */
    int retained = 0;
    if (cache->base_sql != NULL && cache->num_ckpts > 0) {
        while (retained < cache->num_ckpts) {
            occam_sqlite_ckpt *slot = &cache->ckpts[retained];
            if (slot->prefix_len <= sql_len &&
                memcmp(cache->base_sql, sql, slot->prefix_len) == 0) {
                retained++;
            } else {
                break;
            }
        }
        for (int i = retained; i < cache->num_ckpts; i++) {
            if (cache->ckpts[i].db) {
                sqlite3_close(cache->ckpts[i].db);
                cache->ckpts[i].db = NULL;
            }
            free(cache->ckpts[i].end_offsets);
            cache->ckpts[i].end_offsets = NULL;
            free(cache->ckpts[i].result_flags);
            cache->ckpts[i].result_flags = NULL;
        }
        cache->num_ckpts = retained;
    }

    if (retained > 0) {
        if (!cache_ensure_sql_buf(cache, sql, sql_len)) {
            return 0;
        }
        uint8_t *new_base = (uint8_t *)realloc(cache->base_sql, sql_len);
        if (new_base) {
            cache->base_sql = new_base;
            memcpy(cache->base_sql, sql, sql_len);
            cache->base_len = sql_len;
            return 1;
        }
    }

    occam_sqlite_cache_clear_ckpts(cache);
    if (!cache_ensure_sql_buf(cache, sql, sql_len)) {
        return 0;
    }
    cache->base_sql = (uint8_t *)malloc(sql_len);
    if (!cache->base_sql) {
        return 0;
    }
    memcpy(cache->base_sql, sql, sql_len);
    cache->base_len = sql_len;

    int *offsets = (int *)malloc((size_t)total_stmts * sizeof(int));
    uint8_t *flags = (uint8_t *)malloc((size_t)total_stmts);
    if (!offsets || !flags) {
        free(offsets);
        free(flags);
        occam_sqlite_cache_clear_ckpts(cache);
        return 0;
    }

    /* Choose up to OCCAM_MAX_CKPTS evenly spaced statement indices strictly < total_stmts - 1. */
    int step = total_stmts / (OCCAM_MAX_CKPTS + 1);
    if (step < 4) {
        step = 4;
    }
    int next_target = step;

    *dirty_ptr = 0;
    const char *base = cache->sql_buf;
    const char *end = base + sql_len;
    const char *start = base;
    int ordinal = 0;
    int first_result = -1;
    int last_result = -1;
    int has_result = 0;
    int last_matched = 0;
    int sql_limit = sqlite3_limit(db, 1, -1);

    while (start < end && ordinal < total_stmts) {
        sqlite3_stmt *stmt = NULL;
        const char *tail = start;
        ptrdiff_t rem = end - start;
        int nbyte = rem <= sql_limit && rem < INT_MAX
            ? (int)rem + 1
            : (rem > INT_MAX ? INT_MAX : (int)rem);
        int rc = sqlite3_prepare_v2(db, start, nbyte, &stmt, &tail);
        if (rc != 0) {
            break;
        }
        if (stmt == NULL) {
            if (tail <= start) {
                break;
            }
            start = tail;
            continue;
        }
        offsets[ordinal] = (int)(tail - base);
        if (sqlite3_column_count(stmt) == 0) {
            while ((rc = sqlite3_step(stmt)) == 100) {
            }
            sqlite3_finalize(stmt);
            if (rc != 101) {
                break;
            }
            flags[ordinal] = 0;
        } else {
            int stmt_matched = 0;
            int step_rc = 0;
            int mrc = match_stmt_against_expected(stmt, expected_buf, expected_len, &stmt_matched, &step_rc);
            sqlite3_finalize(stmt);
            if (mrc != 0) {
                break;
            }
            has_result = 1;
            if (first_result < 0) {
                first_result = ordinal;
            }
            last_result = ordinal;
            last_matched = stmt_matched;
            flags[ordinal] = (uint8_t)(1 | (stmt_matched ? 2 : 0));
        }

        int dirty = *dirty_ptr;
        int completed_stmts = ordinal + 1;
        size_t prefix_len = (size_t)(tail - base);

        /* Snapshot only at clean, autocommit, semicolon-terminated boundaries. */
        if (completed_stmts >= next_target &&
            completed_stmts < total_stmts &&
            cache->num_ckpts < OCCAM_MAX_CKPTS &&
            prefix_len > 0 &&
            sql[prefix_len - 1] == ';' &&
            (dirty & (OCCAM_DIRTY_TEMP | OCCAM_DIRTY_PRAGMA | OCCAM_DIRTY_CONN_COUNTER | OCCAM_DIRTY_NONDET)) == 0 &&
            sqlite3_get_autocommit(db) == 1) {
            sqlite3 *snap = NULL;
            if (sqlite3_open(":memory:", &snap) == 0 && snap != NULL) {
                if (backup_db_main(snap, db)) {
                    int *off_copy = (int *)malloc((size_t)completed_stmts * sizeof(int));
                    uint8_t *flg_copy = (uint8_t *)malloc((size_t)completed_stmts);
                    if (off_copy && flg_copy) {
                        memcpy(off_copy, offsets, (size_t)completed_stmts * sizeof(int));
                        memcpy(flg_copy, flags, (size_t)completed_stmts);
                        occam_sqlite_ckpt *slot = &cache->ckpts[cache->num_ckpts++];
                        slot->db = snap;
                        slot->prefix_len = prefix_len;
                        slot->stmt_count = completed_stmts;
                        slot->first_result_idx = first_result;
                        slot->last_result_idx = last_result;
                        slot->has_result = has_result;
                        slot->last_matched = last_matched;
                        slot->had_dml = (dirty & OCCAM_DIRTY_DML) != 0 ? 1 : 0;
                        slot->end_offsets = off_copy;
                        slot->result_flags = flg_copy;
                        next_target = completed_stmts + step;
                    } else {
                        free(off_copy);
                        free(flg_copy);
                        sqlite3_close(snap);
                    }
                } else {
                    sqlite3_close(snap);
                }
            }
        }

        ordinal++;
        if (tail <= start) {
            break;
        }
        start = tail;
    }

    free(offsets);
    free(flags);
    return occam_sqlite_reset(db, tmpl, dirty_ptr);
}

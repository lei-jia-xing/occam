#include <stddef.h>
#include <stdint.h>

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
                *dirty |= OCCAM_DIRTY_TEMP;
            }
            return 0;
        case 24: /* SQLITE_ATTACH */
        case 25: /* SQLITE_DETACH */
        case 29: /* SQLITE_CREATE_VTABLE */
        case 30: /* SQLITE_DROP_VTABLE */
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
            if (op == 31 && arg2 != NULL
                && (ascii_eq_ci(arg2, "last_insert_rowid")
                    || ascii_eq_ci(arg2, "changes")
                    || ascii_eq_ci(arg2, "total_changes"))) {
                *dirty |= OCCAM_DIRTY_CONN_COUNTER;
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

extern int sqlite3_get_autocommit(sqlite3 *);
extern int sqlite3_exec(sqlite3 *, const char *sql, int (*callback)(void *, int, char **, char **), void *, char **errmsg);
extern sqlite3_backup *sqlite3_backup_init(sqlite3 *pDest, const char *zDestName, sqlite3 *pSource, const char *zSourceName);
extern int sqlite3_backup_step(sqlite3_backup *p, int nPage);
extern int sqlite3_backup_finish(sqlite3_backup *p);
extern int sqlite3_prepare_v2(sqlite3 *db, const char *zSql, int nByte, sqlite3_stmt **ppStmt, const char **pzTail);
extern int sqlite3_column_count(sqlite3_stmt *pStmt);
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
    const char *start,
    const char *end,
    int ordinal,
    const char **out_tail,
    sqlite3_stmt **out_stmt,
    int *out_ordinal
) {
    while (start < end) {
        sqlite3_stmt *stmt = NULL;
        const char *tail = start;
        ptrdiff_t rem = end - start;
        int nbyte = rem > 0x7fffffff ? 0x7fffffff : (int)rem;
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

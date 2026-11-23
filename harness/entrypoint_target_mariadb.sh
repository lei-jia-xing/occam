#!/usr/bin/env bash
# entrypoint_target_mariadb.sh - Lifecycle manager and crash supervisor for the MariaDB debug container
set -eo pipefail

export PATH="/usr/local/mysql/bin:${PATH}"
DATADIR="${MYSQL_DATADIR:-/var/lib/mysql}"
TEMPLATE_DIR="/var/lib/mysql_template"
LOG_DIR="/var/log/mysql"
ERROR_LOG="${LOG_DIR}/error.log"
RUN_DIR="/var/run/mysqld"

mkdir -p "$DATADIR" "$LOG_DIR" "$RUN_DIR"
touch "$ERROR_LOG"
chown -R mysql:mysql "$DATADIR" "$LOG_DIR" "$RUN_DIR"
chmod 777 "$RUN_DIR"
# Keep the error log readable from a host bind mount.
chmod 0644 "$ERROR_LOG"

if [ "${1:-mysqld}" != "mysqld" ]; then
    exec "$@"
fi

# Restore from template if datadir is empty (e.g. fresh volume or tmpfs)
if [ ! -d "${DATADIR}/mysql" ]; then
    echo "[entrypoint_target_mariadb] Restoring pristine datadir from template..."
    cp -a "${TEMPLATE_DIR}/." "${DATADIR}/"
    chown -R mysql:mysql "$DATADIR"
fi

stop_requested=0
child_pid=""

cleanup() {
    echo "[entrypoint_target_mariadb] Stopping mysqld..."
    stop_requested=1
    if [ -n "$child_pid" ]; then
        kill -TERM "$child_pid" 2>/dev/null || true
        wait "$child_pid" 2>/dev/null || true
    fi
    exit 0
}

trap cleanup SIGTERM SIGINT

# Supervisor loop: relaunch mysqld after assertion aborts or crashes. The
# "mysqld terminated (exit status: N)" line is part of the crash-oracle
# contract shared with the MySQL target.
echo "[entrypoint_target_mariadb] Starting mysqld supervisor..."
while [ "$stop_requested" -eq 0 ]; do
    gosu mysql mysqld \
        --datadir="$DATADIR" \
        --socket="${RUN_DIR}/mysqld.sock" \
        --log-error="$ERROR_LOG" \
        --log-warnings=2 \
        --bind-address=0.0.0.0 \
        --port=3306 &
    child_pid=$!

    # Keep the real exit status; `wait ... || true` would hide crashes.
    if wait "$child_pid"; then
        exit_status=0
    else
        exit_status=$?
    fi

    if [ "$stop_requested" -eq 1 ]; then
        break
    fi

    echo "[entrypoint_target_mariadb] mysqld terminated (exit status: $exit_status). Restarting server for next test..." | tee -a "$ERROR_LOG"
    sleep 0.3
done
